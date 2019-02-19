module U = App_visualise_util;

[@bs.val] external requestAnimationFrame: (float => unit) => int = "";
[@bs.val] external cancelAnimationFrame: int => unit = "";

type assignments =
  | Waiting
  | Loading
  | Error(string)
  | NoInstance
  | FoundInstance(list(App_types.assignment));

type state = {
  simNodes: ref(U.sim_nodes),
  t0: float,
  tNow: float,
  animateReqId: option(int),
  assignments,
};

type action =
  | UpdateTime(float);

let component = ReasonReact.reducerComponent("App_visualise");

let s = ReasonReact.string;

let make = (~assignments, _children) => {
  ...component,
  initialState: () => {
    {
      t0: Js.Date.now(),
      tNow: Js.Date.now(),
      assignments,
      simNodes:
        ref(
          switch (assignments) {
          | FoundInstance(x) => U.merge_with_sim_nodes(U.empty_sim_nodes, x)
          | _ => U.empty_sim_nodes
          },
        ),
      animateReqId:
        switch (assignments) {
        | FoundInstance(_) =>
          Some(requestAnimationFrame(dt => Js.Console.log(dt)))
        | _ => None
        },
    };
  },
  reducer: (action, state) =>
    switch (action) {
    | UpdateTime(tNow) => ReasonReact.Update({...state, tNow})
    },
  willReceiveProps: self => {
    switch (assignments) {
    | FoundInstance(x) =>
      self.state.simNodes := U.merge_with_sim_nodes(self.state.simNodes^, x)
    | _ => self.state.simNodes := U.empty_sim_nodes
    };

    let animateReqId =
      switch (assignments, self.state.animateReqId) {
      | (FoundInstance(_), None) =>
        Some(requestAnimationFrame(dt => Js.Console.log(dt)))
      | (FoundInstance(_), Some(_reqId)) => self.state.animateReqId
      | (_, Some(reqId)) =>
        cancelAnimationFrame(reqId);
        None;
      | _ => self.state.animateReqId
      };

    {...self.state, assignments, animateReqId};
  },
  render: self => {
    let _cx =
      string_of_int(
        int_of_float((self.state.tNow -. self.state.t0) /. 100.),
      );
    <div>
      {switch (self.state.assignments) {
       | Waiting => <div> {s("waiting")} </div>
       | Loading => <div> {s("loading")} </div>
       | Error(x) => <div> {s(Printf.sprintf("Error: %s", x))} </div>
       | NoInstance => <div> {s("no instance")} </div>
       | FoundInstance(_) => <svg width="900" height="500" />
       }}
    </div>;
  },
};
