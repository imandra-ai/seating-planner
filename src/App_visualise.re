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
  animating: ref(bool),
  t0: float,
  tNow: float,
  assignments,
};

type action =
  | UpdateTime(float);

let component = ReasonReact.reducerComponent("App_visualise");

let s = ReasonReact.string;

let rec runAnimation = (animating: ref(bool)) => {
  let _ =
    requestAnimationFrame(_dt =>
      if (animating^) {
        runAnimation(animating);
        ();
      }
    );
  ();
};

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
      animating: {
        let r = ref(false);
        switch (assignments) {
        | FoundInstance(_) =>
          r := true;
          runAnimation(r);
        | _ => ()
        };

        r;
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

    let shouldBeAnimating =
      switch (assignments, self.state.animating^) {
      | (FoundInstance(_), false) => true
      | (FoundInstance(_), true) =>
        /* already running */
        false
      | (_, true) => false
      | _ => false
      };

    let _ =
      if (! self.state.animating^ && shouldBeAnimating) {
        self.state.animating := true;
        runAnimation(self.state.animating);
      } else if (self.state.animating^ && !shouldBeAnimating) {
        self.state.animating := true;
      };

    {...self.state, assignments};
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
       | FoundInstance(_) =>
         <svg id="seats" width="900" height="500"> <g /> </svg>
       }}
    </div>;
  },
};
