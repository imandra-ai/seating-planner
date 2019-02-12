/* State declaration */
open Css;

let traits = [|"Music Taste", "Met at college", "Met at work"|];

let s = ReasonReact.string;

module PairSet =
  Belt.Id.MakeComparable({
    type t = (Z.t, Z.t);
    let cmp = ((x, y): t, (x', y'): t) => {
      let compare_fst = Z.compare(x, x');
      if (compare_fst != 0) {
        compare_fst;
      } else {
        Z.compare(y, y');
      };
    };
  });

type guest = {
  id: int,
  name: string,
  traits: array(string),
};

type initState =
  | Loading
  | Error(Imandra_client.Error.t)
  | Loaded;

type fetchState =
  | Waiting
  | Loading
  | Error(Imandra_client.Error.t)
  | Loaded;

type state = {
  initState,
  fetchState,
  guestText: string,
  constraintText: string,
  guests: array(guest),
  shouldSitTogether: Belt.Set.t((Z.t, Z.t), PairSet.identity),
  shouldSitApart: Belt.Set.t((Z.t, Z.t), PairSet.identity),
};

/* Action declaration */
type action =
  | GuestTextChanged(string)
  | TogglePairing(int, int)
  | ConstraintTextChanged(string)
  | SetInitState(initState)
  | SetFetchState(fetchState)
  | Submit;

let parseGuests = (s: string): array(guest) =>
  Js.String.split("\n", s)
  ->Belt.Array.mapWithIndex((id, g) => (id, g))
  ->Belt.Array.keepMap(((id, g)) => {
      let parts = Js.String.splitByRe([%re "/, ?/"], g);
      Belt.Array.get(parts, 0)
      ->Belt.Option.flatMap(name =>
          if (String.trim(name) != "") {
            Some({id, name, traits: Belt.Array.sliceToEnd(parts, 1)});
          } else {
            None;
          }
        );
    });

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

let initialGuestText = {|Dave, Drum and Bass, y, y
Matt, G-funk, y, y
Ringo, Rock 'n' Roll, y, y
Paul, World Music, y, y
|};

let initialConstraintText = {|let x = 3;;|};

let normPair = ((a, b)) => (Z.of_int(min(a, b)), Z.of_int(max(a, b)));

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
let setupScriptPath = "src/App_setup.ire";

module D = App_decoders.Decode(Decoders_bs.Decode);
module E = App_decoders.Encode(Decoders_bs.Encode);

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = _children => {
  /* spread the other default fields of component here and override a few */
  ...component,

  didMount: self => {
    let _p =
      Imandra_client.Eval.by_src(
        ~syntax=Imandra_client.Api.Reason,
        ~src=Printf.sprintf({|#use "%s";|}, setupScriptPath),
        serverInfo,
      )
      |> Js.Promise.then_(v => {
           switch (v) {
           | Belt.Result.Ok(_) => self.send(SetInitState(Loaded))
           | Belt.Result.Error(e) => self.send(SetInitState(Error(e)))
           };
           Js.Promise.resolve();
         });
    ();
  },

  initialState: () =>
    {
      initState: Loading,
      fetchState: Waiting,
      guestText: initialGuestText,
      constraintText: initialConstraintText,
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
        {...state, fetchState: s},
        _ =>
          switch (s) {
          | Loading => Js.Console.log(Printf.sprintf("Imandra: loading"))
          | Loaded => Js.Console.log(Printf.sprintf("Imandra: loaded"))
          | Error(e) =>
            Js.Console.error(
              Format.asprintf("Imandra: %a", Imandra_client.Error.pp, e),
            )
          | _ => ()
          },
      )

    | GuestTextChanged(text) =>
      ReasonReact.Update({
        ...state,
        guestText: text,
        guests: parseGuests(text),
      })
    | ConstraintTextChanged(text) =>
      ReasonReact.Update({...state, constraintText: text})
    | TogglePairing(a, b) =>
      Js.Console.log(
        Printf.sprintf(
          "toggled %s, %s",
          string_of_int(a),
          string_of_int(b),
        ),
      );
      ReasonReact.Update(
        if (areTogether(state, (a, b))) {
          setApart(state, (a, b));
        } else if (areApart(state, (a, b))) {
          setNeither(state, (a, b));
        } else {
          setTogether(state, (a, b));
        },
      );
    | Submit =>
      ReasonReact.UpdateWithSideEffects(
        {...state, fetchState: Loading},
        self => {
          let togetherJson =
            Decoders_bs.Encode.encode_string(
              E.pairs,
              Belt.Set.toList(state.shouldSitTogether) /* ->Belt.List.map(((a, b)) => (Z.of_int(a), Z.of_int(b))) */,
            );
          let apartJson =
            Decoders_bs.Encode.encode_string(
              E.pairs,
              Belt.Set.toList(state.shouldSitApart),
              /* ->Belt.List.map(((a, b)) => (Z.of_int(a), Z.of_int(b))), */
            );

          self.send(SetFetchState(Loading));

          let _p =
            Imandra_client.Eval.by_src(
              ~syntax=Imandra_client.Api.Reason,
              ~src=
                Printf.sprintf(
                  {|
                 [@program] let shouldSitTogether: pairList = Decoders_yojson.Basic.Decode.decode_string(D.pairs, "%s") |> CCResult.get_exn;
                 Imandra.port(~var="shouldSitTogether", "shouldSitTogether");
                 [@program] let shouldSitApart: pairList = Decoders_yojson.Basic.Decode.decode_string(D.pairs, "%s") |> CCResult.get_exn;
                 Imandra.port(~var="shouldSitApart", "shouldSitApart");
                 |},
                  togetherJson,
                  apartJson,
                ),
              serverInfo,
            )
            |> Js.Promise.then_(v => {
                 switch (v) {
                 | Belt.Result.Ok(_) => self.send(SetFetchState(Loaded))
                 | Belt.Result.Error(e) =>
                   self.send(SetFetchState(Error(e)))
                 };
                 Js.Promise.resolve();
               });

          ();
        },
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
            {s(
               "Enter your guests below, one per line. Seperate the traits by a comma, e.g:",
             )}
          </Typography>
          <Typography
            className={style([
              marginBottom(px(10)),
              fontFamily("monospace"),
              whiteSpace(`pre),
            ])}>
            {s("Matt, Drum and Bass, y, y")}
          </Typography>
          <TextField
            multiline=true
            rows={`Int(5)}
            rowsMax={`Int(10)}
            placeholder="Enter guest details"
            defaultValue={`String(self.state.guestText)}
            className={style([
              fontFamily("monospace"),
              whiteSpace(`pre),
              width(pct(80.)),
              marginBottom(px(20)),
            ])}
            onChange={e =>
              self.send(GuestTextChanged(ReactEvent.Form.target(e)##value))
            }
          />
          <Table>
            <TableHead>
              <TableRow>
                <TableCell> {s("Name")} </TableCell>
                {ReasonReact.array(
                   Array.map(
                     t => <TableCell key=t> {s(t)} </TableCell>,
                     traits,
                   ),
                 )}
              </TableRow>
            </TableHead>
            <TableBody>
              {let strTraitOrDash = (g, i) =>
                 g.traits
                 ->Belt.Array.get(i)
                 ->Belt.Option.getWithDefault("-")
                 ->s

               let boolTraitOrDash = (g, i) =>
                 g.traits
                 ->Belt.Array.get(i)
                 ->Belt.Option.map(
                     fun
                     | "yes"
                     | "y"
                     | "true" =>
                       <MaterialUi_Icons icon=`Check fontSize=`Small />
                     | "no"
                     | "n"
                     | "false" =>
                       <MaterialUi_Icons icon=`Close fontSize=`Small />
                     | _ =>
                       <MaterialUi_Icons icon=`ErrorOutline fontSize=`Small />,
                   )
                 ->Belt.Option.getWithDefault(<div> {s("-")} </div>)

               ReasonReact.array(
                 Array.map(
                   g =>
                     <TableRow key={string_of_int(g.id)}>
                       <TableCell> {g.name} </TableCell>
                       <TableCell> {strTraitOrDash(g, 0)} </TableCell>
                       <TableCell> {boolTraitOrDash(g, 1)} </TableCell>
                       <TableCell> {boolTraitOrDash(g, 2)} </TableCell>
                     </TableRow>,
                   self.state.guests,
                 ),
               )}
            </TableBody>
          </Table>
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
                   Array.map(
                     g =>
                       <TableCell key={string_of_int({g.id})}>
                         <div className={style([paddingLeft(px(8))])}>
                           {s(g.name)}
                         </div>
                       </TableCell>,
                     self.state.guests,
                   ),
                 )}
              </TableRow>
            </TableHead>
            <TableBody>
              {ReasonReact.array(
                 self.state.guests
                 ->Belt.Array.map(gRow =>
                     <TableRow key={string_of_int({gRow.id})}>
                       <TableCell> {gRow.name} </TableCell>
                       {ReasonReact.array(
                          self.state.guests
                          ->Belt.Array.map(gCol =>
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
                            ),
                        )}
                     </TableRow>
                   ),
               )}
            </TableBody>
          </Table>
        </Paper>
        <Paper className=paperStyles>
          <Typography variant=`H4 className=paperHeadingStyles>
            {s("Your constraints")}
          </Typography>
          <TextField
            multiline=true
            rows={`Int(20)}
            rowsMax={`Int(20)}
            placeholder="Enter your seating constraints"
            defaultValue={`String(self.state.constraintText)}
            className={style([
              fontFamily("monospace"),
              whiteSpace(`pre),
              width(pct(80.)),
              marginBottom(px(20)),
            ])}
            onChange={e =>
              self.send(
                ConstraintTextChanged(ReactEvent.Form.target(e)##value),
              )
            }
          />
          <Button onClick={_e => self.send(Submit)}> {s("Submit")} </Button>
          <Typography className={style([marginBottom(px(10))])}>
            {switch (self.state.fetchState) {
             | Waiting => s("waiting")
             | Loading => s("loading")
             | Error(e) =>
               s(Format.asprintf("error: %a", Imandra_client.Error.pp, e))
             | Loaded => s("loaded")
             }}
          </Typography>
        </Paper>
        <Paper className=paperStyles>
          <Typography variant=`H4 className=paperHeadingStyles>
            {s("The seating arrangement")}
          </Typography>
          <svg id="seats" width="900" height="500" />
        </Paper>
      </main>
    ),
};
