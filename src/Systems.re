
let component = ReasonReact.reducerComponent("Systems");

open nks.Model;

type state =
  | Loading
  | Error(err)
  | Loaded(array(system));

type action =
  | GetSystems
  | SystemsRetrieved(array(system));


let make = () => {
  ...component,
  didMount: self => self.send(GetSystems),

  reducer: (action, _state) =>
    switch(action) {
      | GetSystems =>
        ReasonReact.UpdateWithSideEffects(
          Loading,
          (
            self =>
              Js.Promise.(
                nks.Model.API.get_systems()
                |> then_(systems =>
                    systems
                    |> SystemsRetrieved
                    |> self.send
                    |> resolve
                  )
                |> catch(err =>
                  Js.Promise.resolve(self.send(Error(err)))
                )
                |> ignore
              )

          )
        )
    }
};
