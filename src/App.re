/* State declaration */
open Css;
open App_types;

let s = ReasonReact.string;

module PairSet =
  Belt.Id.MakeComparable({
    type t = (int, int);
    let cmp = ((x, y): t, (x', y'): t) => {
      let compare_fst = compare(x, x');
      if (compare_fst != 0) {
        compare_fst;
      } else {
        compare(y, y');
      };
    };
  });

type initState =
  | Loading
  | Error(Imandra_client.Error.t)
  | Loaded;

type reqId = int;

type fetchState =
  | Waiting
  | Loading(reqId)
  | Error(reqId, string)
  | FoundInstance(reqId, list(assignment))
  | NoInstance(reqId);

type state = {
  initState,
  fetchState,
  lastReqId: reqId,
  guestText: string,
  guests: list(guest),
  guestsPerTable: int,
  numberOfTables: int,
  shouldSitTogether: Belt.Set.t((int, int), PairSet.identity),
  shouldSitApart: Belt.Set.t((int, int), PairSet.identity),
};

/* Action declaration */
type action =
  | GuestTextChanged(string)
  | GuestsPerTableChanged(int)
  | NumberOfTablesChanged(int)
  | TogglePairing(int, int)
  | SetInitState(initState)
  | SetFetchState(fetchState);

let parseGuests = (s: string): list(guest) =>
  Js.String.splitByRe([%re "/, ?/"], s)
  ->Belt.Array.mapWithIndex((id, name) => {id, name})
  ->Belt.Array.keepMap(g =>
      if (String.trim(g.name) != "") {
        Some(g);
      } else {
        None;
      }
    )
  ->Array.to_list;

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("App");

let initialGuestText = {|John, Paul, George, Ringo, Jack, Jones, Jonah, Jonny|};

let normPair = ((a, b)) => (min(a, b), max(a, b));

let areTogether = (s, pair) =>
  Belt.Set.has(s.shouldSitTogether, normPair(pair));

let areApart = (s, pair) => Belt.Set.has(s.shouldSitApart, normPair(pair));

let setTogether = (s, pair) => {
  ...s,
  shouldSitApart: Belt.Set.remove(s.shouldSitApart, normPair(pair)),
  shouldSitTogether: Belt.Set.add(s.shouldSitTogether, normPair(pair)),
};

let setApart = (s, pair) => {
  ...s,
  shouldSitApart: Belt.Set.add(s.shouldSitApart, normPair(pair)),
  shouldSitTogether: Belt.Set.remove(s.shouldSitTogether, normPair(pair)),
};

let setNeither = (s, pair) => {
  ...s,
  shouldSitApart: Belt.Set.remove(s.shouldSitApart, normPair(pair)),
  shouldSitTogether: Belt.Set.remove(s.shouldSitTogether, normPair(pair)),
};

let paperStyles = style([padding(px(20)), marginTop(px(20))]);
let paperHeadingStyles = style([marginBottom(px(20))]);

let serverInfo: Imandra_client.Server_info.t = {url: "http://localhost:3000"};
let setupScriptPath = "src/Server_setup.iml";

module D = App_decoders.Decode(Decoders_bs.Decode);
module E = App_decoders.Encode(Decoders_bs.Encode);

let sendToImandra = (state, send) => {
  let togetherJson =
    Decoders_bs.Encode.encode_string(
      E.intPairs,
      Belt.Set.toList(state.shouldSitTogether),
    );
  let apartJson =
    Decoders_bs.Encode.encode_string(
      E.intPairs,
      Belt.Set.toList(state.shouldSitApart),
    );

  let totalGuests = List.length(state.guests);

  let reqId = state.lastReqId + 1;

  send(SetFetchState(Loading(reqId)));

  let _p =
    Imandra_client.Eval.by_src(
      ~syntax=Imandra_client.Api.OCaml,
      ~src=
        Printf.sprintf(
          {|
#redef true;;
let should_sit_together: pairList = (Decoders_yojson.Basic.Decode.decode_string D.zPairs %S) |> CCResult.get_exn [@@program];;
let _ = Imandra.port ~var:"should_sit_together" "should_sit_together";;
let should_sit_apart: pairList = (Decoders_yojson.Basic.Decode.decode_string D.zPairs %S) |> CCResult.get_exn [@@program];;
let _ = Imandra.port ~var:"should_sit_apart" "should_sit_apart";;
let total_guests = %d;;
let max_guests_per_table = %d;;
let number_of_tables = %d;;
print_endline ("let should_sit_together = [" ^ CCString.concat "; " (should_sit_together |> CCList.map (fun (a, b) -> "(" ^ Z.to_string(a) ^ "," ^ Z.to_string(b) ^ ")" )) ^ "]") [@@program];;
print_endline ("let should_sit_apart = [" ^ CCString.concat "; " (should_sit_apart |> CCList.map (fun (a, b) -> "(" ^ Z.to_string(a) ^ "," ^ Z.to_string(b) ^ ")" )) ^ "]") [@@program];;
print_endline ("let total_guests = " ^ Z.to_string(total_guests)) [@@program];;
print_endline ("let max_guests_per_table = " ^ Z.to_string(max_guests_per_table)) [@@program];;
print_endline ("let number_of_tables = " ^ Z.to_string(number_of_tables)) [@@program];;
let print_pairs_for_total_guests = print_pairs total_guests [@@program];;
         |},
          togetherJson,
          apartJson,
          totalGuests,
          state.guestsPerTable,
          state.numberOfTables,
        ),
      serverInfo,
    )
    |> Js.Promise.then_(v => {
         switch (v) {
         | Belt.Result.Ok(_) =>
           let _p =
             Imandra_client.Instance.by_src(
               ~hints={method_: Unroll({steps: Some(300)})},
               ~instance_printer={
                 name: "print_pairs_for_total_guests",
                 cx_var_name: "x",
               },
               ~syntax=Imandra_client.Api.OCaml,
               ~src=
                 "(fun x -> valid_assignment x should_sit_together should_sit_apart total_guests max_guests_per_table number_of_tables)",
               serverInfo,
             )
             |> Js.Promise.then_(v => {
                  switch (v) {
                  | Belt.Result.Ok(Imandra_client.Api.Response.I_sat(i)) =>
                    switch (i.instance.printed) {
                    | None =>
                      send(
                        SetFetchState(
                          Error(reqId, "Instance found, but not printed"),
                        ),
                      )
                    | Some(p) =>
                      switch (Decoders_bs.Decode.decode_string(D.intPairs, p)) {
                      | Ok(pairs) =>
                        send(
                          SetFetchState(
                            FoundInstance(
                              reqId,
                              pairs->Belt.List.map(((p, t)) => {
                                let guest =
                                  state.guests
                                  |> List.find((g: guest) => g.id == p);
                                {guest, table: t};
                              }),
                            ),
                          ),
                        )
                      | Error(e) =>
                        send(
                          SetFetchState(
                            Error(
                              reqId,
                              Format.asprintf(
                                "%a",
                                Decoders_bs.Decode.pp_error,
                                e,
                              ),
                            ),
                          ),
                        )
                      }
                    }

                  | Belt.Result.Ok(_) =>
                    send(SetFetchState(NoInstance(reqId)))
                  | Belt.Result.Error(e) =>
                    send(
                      SetFetchState(
                        Error(reqId, Imandra_client.Error.pp_str(e)),
                      ),
                    )
                  };
                  Js.Promise.resolve();
                });
           ();
         | Belt.Result.Error(e) =>
           send(
             SetFetchState(Error(reqId, Imandra_client.Error.pp_str(e))),
           )
         };
         Js.Promise.resolve();
       });
  ();
};

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = _children => {
  /* spread the other default fields of component here and override a few */
  ...component,

  didMount: self => {
    let _p =
      Imandra_client.Eval.by_src(
        ~syntax=Imandra_client.Api.OCaml,
        ~src=Printf.sprintf({|#use "%s"|}, setupScriptPath),
        serverInfo,
      )
      |> Js.Promise.then_(v => {
           switch (v) {
           | Belt.Result.Ok(_) => self.send(SetInitState(Loaded))
           | Belt.Result.Error(e) => self.send(SetInitState(Error(e)))
           };
           Js.Promise.resolve();
         });
    sendToImandra(self.state, self.send);
    ();
  },

  initialState: () =>
    {
      initState: Loading,
      lastReqId: 0,
      fetchState: Waiting,
      guestText: initialGuestText,
      guestsPerTable: 4,
      numberOfTables: 2,
      guests: parseGuests(initialGuestText),
      shouldSitTogether: Belt.Set.make(~id=(module PairSet)),
      shouldSitApart: Belt.Set.make(~id=(module PairSet)),
    }
    ->setTogether((1, 3))
    ->setApart((0, 2)),

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | SetInitState(s) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, initState: s},
        _ =>
          switch (s) {
          | Loaded =>
            Js.Console.log(
              Printf.sprintf(
                "Imandra init: Loaded file: %s",
                setupScriptPath,
              ),
            )
          | Error(e) =>
            Js.Console.error(
              Format.asprintf("Imandra init: %a", Imandra_client.Error.pp, e),
            )
          | _ => ()
          },
      )
    | SetFetchState(s) =>
      ReasonReact.UpdateWithSideEffects(
        switch (state.fetchState, s) {
        | (_, Loading(id)) => {...state, lastReqId: id, fetchState: s}
        | (Loading(a), Error(b, _)) when a == b => {...state, fetchState: s}
        | (Loading(a), FoundInstance(b, _)) when a == b => {
            ...state,
            fetchState: s,
          }
        | (Loading(a), NoInstance(b)) when a == b => {
            ...state,
            fetchState: s,
          }
        | _ => state
        },
        self => {
          switch (s) {
          | Loading(id) =>
            Js.Console.log(Printf.sprintf("Imandra[%d]: loading", id))
          | NoInstance(id) =>
            Js.Console.log(Printf.sprintf("Imandra[%d]: No instance", id))
          | FoundInstance(id, _) =>
            Js.Console.log(Printf.sprintf("Imandra[%d]: Found instance", id))
          | Error(id, e) =>
            Js.Console.error(Printf.sprintf("Imandra[%d]: %s", id, e))
          | _ => ()
          };

          switch (self.state.fetchState) {
          | FoundInstance(id, _assignments) when self.state.fetchState == s =>
            Js.Console.log(Printf.sprintf("Imandra[%d]: Updating graph", id))

          /* App_visualise.handle_new_assignments("#seats", assignments); */
          /* update graph when the state has actually been updated by the incoming action */
          /* let new_nodes = Visualise.nodes_of_assignments(assignments); */

          /* if new_nodes == ^graph_nodes then */
          /* clear_graph (); */

          /* forceGraph( */
          /*   "#seats", */
          /*   Decoders_bs.Encode.encode_value(E.assignments, assignments), */
          /* ); */
          | _ => ()
          };
        },
      )

    | GuestTextChanged(text) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, guestText: text, guests: parseGuests(text)},
        self => sendToImandra(self.state, self.send),
      )
    | GuestsPerTableChanged(c) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, guestsPerTable: c},
        self => sendToImandra(self.state, self.send),
      )
    | NumberOfTablesChanged(c) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, numberOfTables: c},
        self => sendToImandra(self.state, self.send),
      )
    | TogglePairing(a, b) =>
      ReasonReact.UpdateWithSideEffects(
        if (areTogether(state, (a, b))) {
          setApart(state, (a, b));
        } else if (areApart(state, (a, b))) {
          setNeither(state, (a, b));
        } else {
          setTogether(state, (a, b));
        },
        self => sendToImandra(self.state, self.send),
      )
    },

  render: self =>
    MaterialUi.(
      <main
        className={style([
          width(px(960)),
          marginLeft(auto),
          marginRight(auto),
          padding(px(20)),
        ])}>
        <CssBaseline />
        <Typography variant=`H2 className={style([marginBottom(px(20))])}>
          {s("Seating planner")}
        </Typography>
        <Paper className=paperStyles>
          <Typography variant=`H4 className=paperHeadingStyles>
            {s("Guests")}
          </Typography>
          <Typography className={style([marginTop(px(10))])}>
            {s("Enter the names of your guests below, seperated by a comma:")}
          </Typography>
          <TextField
            multiline=true
            rows={`Int(5)}
            rowsMax={`Int(10)}
            placeholder="Enter guest names"
            defaultValue={`String(self.state.guestText)}
            className={style([
              width(pct(80.)),
              marginTop(px(20)),
              marginBottom(px(20)),
              fontWeight(bold),
            ])}
            onChange={e =>
              self.send(GuestTextChanged(ReactEvent.Form.target(e)##value))
            }
          />
          <Table className={style([marginBottom(px(30))])}>
            <TableHead>
              <TableRow>
                <TableCell className={style([width(px(20))])}>
                  {s("ID")}
                </TableCell>
                <TableCell> {s("Name")} </TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {ReasonReact.array(
                 self.state.guests
                 ->Belt.List.map(g =>
                     <TableRow key={string_of_int(g.id)}>
                       <TableCell> {g.id} </TableCell>
                       <TableCell> {g.name} </TableCell>
                     </TableRow>
                   )
                 ->Array.of_list,
               )}
            </TableBody>
          </Table>
          <Typography variant=`H5>
            {s("Number of guests per table")}
          </Typography>
          <TextField
            placeholder="Enter number of guests per table"
            defaultValue={`Int(self.state.guestsPerTable)}
            type_="number"
            className={style([
              width(pct(80.)),
              marginTop(px(20)),
              marginBottom(px(20)),
              fontWeight(bold),
            ])}
            onChange={e =>
              self.send(
                GuestsPerTableChanged(ReactEvent.Form.target(e)##value),
              )
            }
          />
          <Typography variant=`H5> {s("Number of tables")} </Typography>
          <TextField
            placeholder="Enter number of tables"
            defaultValue={`Int(self.state.numberOfTables)}
            type_="number"
            className={style([
              width(pct(80.)),
              marginTop(px(20)),
              marginBottom(px(20)),
              fontWeight(bold),
            ])}
            onChange={e =>
              self.send(
                NumberOfTablesChanged(ReactEvent.Form.target(e)##value),
              )
            }
          />
        </Paper>
        <Paper className=paperStyles>
          <Typography variant=`H4 className=paperHeadingStyles>
            {s("Pairings")}
          </Typography>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell> {s("Guest")} </TableCell>
                {ReasonReact.array(
                   self.state.guests
                   ->Belt.List.map(g =>
                       <TableCell key={string_of_int({g.id})}>
                         <div className={style([paddingLeft(px(8))])}>
                           {s(g.name)}
                         </div>
                       </TableCell>
                     )
                   ->Array.of_list,
                 )}
              </TableRow>
            </TableHead>
            <TableBody>
              {ReasonReact.array(
                 self.state.guests
                 ->Belt.List.map(gRow =>
                     <TableRow key={string_of_int({gRow.id})}>
                       <TableCell> {gRow.name} </TableCell>
                       {ReasonReact.array(
                          self.state.guests
                          ->Belt.List.map(gCol =>
                              <TableCell
                                key={Printf.sprintf(
                                  "%s-%s",
                                  string_of_int(gRow.id),
                                  string_of_int(gCol.id),
                                )}>
                                {if (gRow.id == gCol.id) {
                                   <IconButton disabled=true>
                                     <MaterialUi_Icons
                                       icon=`Close
                                       fontSize=`Small
                                       color=`Disabled
                                     />
                                   </IconButton>;
                                 } else {
                                   <IconButton
                                     onClick={_e =>
                                       self.send(
                                         TogglePairing(gRow.id, gCol.id),
                                       )
                                     }>
                                     {if (areTogether(
                                            self.state,
                                            (gRow.id, gCol.id),
                                          )) {
                                        <MaterialUi_Icons
                                          icon=`SentimentVerySatisfied
                                          fontSize=`Small
                                          color=`Primary
                                        />;
                                      } else if (areApart(
                                                   self.state,
                                                   (gRow.id, gCol.id),
                                                 )) {
                                        <MaterialUi_Icons
                                          icon=`SentimentVeryDissatisfied
                                          fontSize=`Small
                                          color=`Secondary
                                        />;
                                      } else {
                                        <MaterialUi_Icons
                                          icon=`SentimentSatisfied
                                          fontSize=`Small
                                        />;
                                      }}
                                   </IconButton>;
                                 }}
                              </TableCell>
                            )
                          ->Array.of_list,
                        )}
                     </TableRow>
                   )
                 ->Array.of_list,
               )}
            </TableBody>
          </Table>
        </Paper>
        <Paper className=paperStyles>
          <Typography variant=`H4 className=paperHeadingStyles>
            {s("The seating arrangement")}
          </Typography>
          <App_visualise
            assignments={
              switch (self.state.fetchState) {
              | Waiting => Waiting
              | Loading(_) => Loading
              | Error(_, x) => Error(x)
              | FoundInstance(_, xs) => FoundInstance(xs)
              | NoInstance(_) => NoInstance
              }
            }
          />
        </Paper>
      </main>
    ),
};
