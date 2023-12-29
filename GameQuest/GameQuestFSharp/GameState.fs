module GameState

open Story
open OrchestrationCE.Coordination
open FSharp.Json
open System.IO

type GameState private (storyEvents: StoryEvent list, story: Story) =
    let actions = 
        lazy (
            let { CoordinationResult.Result = actions } = story None 
            actions
        )

    member this.EventExpositions with get () =
        List.choose (function | Exposition (ex, Some e) -> Some (ex, e) | _ -> None) actions.Value

    member this.NonEventExpositions with get () =
        List.choose (function | Exposition (ex, None) -> Some ex | _ -> None) actions.Value

    member this.Battle with get () =
        actions.Value
        |> List.choose (function | Battle state -> Some state | _ -> None)
        |> List.tryHead

    member this.Interactives with get () =
        List.choose (function | Interactive (i, e) -> Some (i, e) | _ -> None) actions.Value

    member this.StoryEvents with get () = storyEvents

    member this.DoEvent (event: StoryEvent) =
        let { Next = next } = story (Some event)
        Option.map (fun next -> GameState (event::storyEvents, next)) next

    member this.Save (filename: string) =
        File.WriteAllText(filename, storyEvents |> Json.serialize)

    static member Load (filename: string, story: Story) =
        let events = 
            File.ReadAllText(filename)
            |> Json.deserialize<Story.StoryEvent list> 

        let loaded = Utilities.loadGame story events

        Option.map (fun loaded -> GameState (events, loaded)) loaded

    static member New (story: Story) = GameState ([], story)