
open MomentRe;

module SystemRow = {
  let component = ReasonReact.statelessComponent("SystemRow");

  let make = (~sys: Model.system, _children) => {
    ...component,
    render: _self => {
      ReasonReact.(
        <tr>
          <td>(string(sys.query))</td>
          <td>(string(
            switch(sys.league) {
              | Model.NFL => "NFL"
              | Model.NCAA => "NCAA"
            }
          ))
          </td>
          <td>(string(sys.description))</td>
          <td>(string(Moment.defaultFormat(sys.last_alerted)))</td>
        </tr>
      );
    }
  }
}

module SystemsTableHeader = {
  let component = ReasonReact.statelessComponent("SystemsTableHeader");

  let make = (~systems, _)=> {
    ...component,
    render: _self => {
      <table>
        <thead>
          <tr>
            <td>(ReasonReact.string("Query"))</td>
            <td>(ReasonReact.string("League"))</td>
            <td>(ReasonReact.string("Description"))</td>
            <td>(ReasonReact.string("Last Alerted"))</td>
          </tr>
        </thead>
        <tbody>
        ...(Js.Array.map(sys => <SystemRow sys />, systems))
        </tbody>
      </table>
    }
  };

}

module SystemsTable = {
  type state =
    | StateLoading
    | StateError(string)
    | StateLoaded(array(Model.system));

  type action =
    | SystemsRequest
    | SystemsSuccess(array(Model.system))
    | SystemsError(string);

  let component = ReasonReact.reducerComponent("Systems");

  let make = _children => {
    ...component,
    initialState: _state => StateLoading,

    didMount: self => self.send(SystemsRequest),

    reducer: (action, _state) => {
      switch(action) {
        | SystemsRequest =>
          ReasonReact.UpdateWithSideEffects(
            StateLoading,
            (
              self =>
                Js.Promise.(
                  Model.API.get_systems()
                  /* |> then_(Fetch.Response.json)
                  |> then_(json =>
                      json
                      |> Model.Decode.systems_array
                      |> (sys => self.send(SystemsSuccess(sys)))
                      |> resolve
                    ) */
                  |> then_(xs => {
                    resolve(self.send(SystemsSuccess(xs)));
                  })
                  |> catch(err => {
                      Js.log(err);
                      Js.Promise.resolve(self.send(SystemsError("error fetching")))
                  })
                  |> ignore
                )
            ),
          )
        | SystemsSuccess(systems) => ReasonReact.Update(StateLoaded(systems))
        | SystemsError(e) => ReasonReact.Update(StateError(e))
      }
    },

    render: self =>
      switch (self.state) {
        | StateError(ex) => <div> (ReasonReact.string(ex)) </div>
        | StateLoading => <div> (ReasonReact.string("Loading...")) </div>
        | StateLoaded(systems) => <div> <SystemsTableHeader systems/> </div>
      }
    }
};

