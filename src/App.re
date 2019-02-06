/* State declaration */
open Css;

let traits = [|"Music Taste", "Met at college", "Met at work"|];

type state = {guests: array(array(string))};

/* Action declaration */
type action =
  | GuestTextChanged(string);

let parseGuestText = (s: string) => {
  Js.String.split("\n", s)
  |> Array.map(p => Js.String.splitByRe([%re "/, ?/"], p));
};

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = _children => {
  /* spread the other default fields of component here and override a few */
  ...component,

  initialState: () => {guests: [||]},

  /* State transitions */
  reducer: (action, _state) =>
    switch (action) {
    | GuestTextChanged(text) =>
      ReasonReact.Update({guests: parseGuestText(text)})
    },

  render: self => {
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
          {ReasonReact.string("Seating planner")}
        </Typography>
        <Paper className={style([padding(px(20))])}>
          <Typography variant=`H4>
            {ReasonReact.string("Guests")}
          </Typography>
          <Typography className={style([marginTop(px(10))])}>
            {ReasonReact.string(
               "Enter your guests below, one per line. Seperate the traits by a comma, e.g:",
             )}
          </Typography>
          <Typography
            className={style([
              marginBottom(px(10)),
              fontFamily("monospace"),
              whiteSpace(`pre),
            ])}>
            {ReasonReact.string("Matt, Drum and Bass, y, y")}
          </Typography>
          <TextField
            multiline=true
            rows={`Int(5)}
            rowsMax={`Int(10)}
            placeholder="Enter guest details"
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
                <TableCell> {ReasonReact.string("Name")} </TableCell>
                {ReasonReact.array(
                   Array.map(
                     t => <TableCell> {ReasonReact.string(t)} </TableCell>,
                     traits,
                   ),
                 )}
              </TableRow>
            </TableHead>
            <TableBody>
              {let orDash = (g, i) =>
                 g
                 ->Belt.Array.get(i)
                 ->Belt.Option.getWithDefault("-")
                 ->ReasonReact.string

               let boolOrDash = (g, i) =>
                 g
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
                 ->Belt.Option.getWithDefault(
                     <div> {ReasonReact.string("-")} </div>,
                   )

               ReasonReact.array(
                 Array.map(
                   g =>
                     <TableRow>
                       <TableCell> {orDash(g, 0)} </TableCell>
                       <TableCell> {orDash(g, 1)} </TableCell>
                       <TableCell> {boolOrDash(g, 2)} </TableCell>
                       <TableCell> {boolOrDash(g, 3)} </TableCell>
                     </TableRow>,
                   self.state.guests,
                 ),
               )}
            </TableBody>
          </Table>
        </Paper>
        <Paper className={style([marginTop(px(20)), padding(px(20))])}>
          <Typography variant=`H4>
            {ReasonReact.string("Tables")}
          </Typography>
        </Paper>
      </main>
    );
  },
};
