module Utilities

open OrchestrationCE
open Coordination
open EventAndState

let baseStoryWithState updateState initState =
    event Some
    |> Coordination.scan (stateAccumulator updateState) ({ Event = None; State = initState })
    |> Coordination.skip 1
    |> Coordination.map (function | { Event = Some e; State = state } -> Some { Event = e; State = state } | _ -> None)

type Action<'TEvent, 'TExposition, 'TInteractive> =
    | Exposition of 'TExposition * ('TEvent option)
    | Interactive of 'TInteractive * 'TEvent

module Action =
    let mapEvent f = function
        | Exposition (msg, event) -> Exposition (msg, event |> Option.map f)
        | Interactive (msg, event) -> Interactive (msg, f event)

    let mapExposition f = function
        | Exposition (msg, event) -> Exposition (f msg, event)
        | Interactive (msg, event) -> Interactive (msg, event)

    let mapInteractive f = function
        | Interactive (msg, event) -> Interactive (f msg, event)
        | Exposition (msg, event) -> Exposition (msg, event)

let loadGame story gameEvents =
    let rec loadGame' story gameEvents =
        match gameEvents with
        | [] -> Some story
        | event::remainingEvents -> 
            let { Next = next } = story (Some event)
            match next with
            | Some next -> loadGame' next remainingEvents
            | _ -> None
    loadGame' story (List.rev gameEvents)