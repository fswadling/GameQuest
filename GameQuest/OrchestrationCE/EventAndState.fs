module OrchestrationCE.EventAndState
open OrchestrationCE.Coordination

type EventAndState<'TState, 'TEvent> = 
    { Event : 'TEvent
      State : 'TState }

let stateAccumulator updateState eventAndState = function
    | Some e -> { State = updateState eventAndState.State e; Event = Some e }
    | None -> { State = eventAndState.State; Event = None }

let chooseOrchestrationEvents chooser = function
    | { Event = Some e } ->
        match chooser e with
        | Some e -> Some (Some e)
        | _ -> None
    | { Event = None } ->
        Some None

let chooseOrchestrationEventAndStates chooser = function
    | { Event = Some e; State = state } ->
        match chooser e with
        | Some e -> Some { Event = Some e; State = state }
        | _ -> None
    | { Event = None; State = state } ->
        Some { Event = None; State = state }

let chooseStateOnGetNextStep = function
    | { Event = None; State = state } -> Some state
    | _ -> None
