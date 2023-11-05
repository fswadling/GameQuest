module Utilities
open OrchestrationCE.Coordination

open Microsoft.Xna.Framework

type MenuJourneyEvent =
    | Initialise
    | TitleScreenDone
    | StartLoadSelected of {| DoLoad: bool |}
    | LoadFileResult of string option
    | Game

type IMenu =
    abstract member Initialise: unit -> unit
    abstract member OnUpdate: GameTime -> unit
    abstract member OnRender: unit -> unit

// I needed to add this class to tame the type signature
// consumed in the C# code.
type StaticCoordinationRunner (coordination) =
    let mutable coordination = Some coordination

    member this.DoStep (e: MenuJourneyEvent): IMenu option =
        let next =
            coordination 
            |> Option.map (fun x -> x (Some e))
            |> Option.bind (fun x -> x.Next)

        coordination <- next

        let result =
            coordination
            |> Option.map (fun x -> x (None))
            |> Option.map (fun x -> x.Result)
            |> Option.defaultValue []

        result |> List.tryHead
