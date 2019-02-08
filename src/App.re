/* State declaration */
open Css;

let traits = [|"Music Taste", "Met at college", "Met at work"|];

let s = ReasonReact.string;

module PairSet =
  Belt.Id.MakeComparable({
    type t = (int, int);
    let cmp = ((x, y), (x', y')) => {
      let compare_fst = compare(x, x');
      if (compare_fst != 0) {
        compare_fst;
      } else {
        compare(y, y');
      };
    };
  });

type guest = {
  id: int,
  name: string,
  traits: array(string),
};

type state = {
  guestText: string,
  guests: array(guest),
  shouldSitTogether: Belt.Set.t((int, int), PairSet.identity),
  shouldSitApart: Belt.Set.t((int, int), PairSet.identity),
};

/* Action declaration */
type action =
  | GuestTextChanged(string)
  | TogglePairing(int, int);

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

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = _children => {
  /* spread the other default fields of component here and override a few */
  ...component,

  initialState: () =>
    {
      guestText: initialGuestText,
      guests: parseGuests(initialGuestText),
      shouldSitTogether: Belt.Set.make(~id=(module PairSet)),
      shouldSitApart: Belt.Set.make(~id=(module PairSet)),
    }
    ->setTogether((1, 3))
    ->setApart((0, 2)),

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | GuestTextChanged(text) =>
      ReasonReact.Update({
        ...state,
        guestText: text,
        guests: parseGuests(text),
      })
    | TogglePairing(a, b) =>
      ReasonReact.Update(
        if (areTogether(state, (a, b))) {
          setApart(state, (a, b));
        } else if (areApart(state, (a, b))) {
          setNeither(state, (a, b));
        } else {
          setTogether(state, (a, b));
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
        <Typography variant=`H2> {s("Seating planner")} </Typography>
        <Paper className={style([padding(px(20)), marginTop(px(40))])}>
          <Typography variant=`H4> {s("Guests")} </Typography>
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
                   Array.map(t => <TableCell> {s(t)} </TableCell>, traits),
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
                     <TableRow>
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
        <Paper className={style([marginTop(px(20)), padding(px(20))])}>
          <Typography variant=`H4> {s("Seating overrides")} </Typography>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell> {s("Guest")} </TableCell>
                {ReasonReact.array(
                   Array.map(
                     g => <TableCell> {g.name} </TableCell>,
                     self.state.guests,
                   ),
                 )}
              </TableRow>
            </TableHead>
            <TableBody>
              {ReasonReact.array(
                 self.state.guests
                 ->Belt.Array.map(gRow =>
                     <TableRow>
                       <TableCell> {gRow.name} </TableCell>
                       {ReasonReact.array(
                          self.state.guests
                          ->Belt.Array.map(gCol =>
                              <TableCell>
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
      </main>
    ),
};
