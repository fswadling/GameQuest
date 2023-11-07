module ScreenJourney

open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open Utilities
open Myra.Graphics2D.UI

let rec mainLoop desktop updateFn = orchestration {
    do! event (function | OpenMenuScreen -> Some () | _ -> None)
        |> raiseToOrchestrationWithActions [new Screens.GameScreen(desktop, updateFn) :> IScreen]

    do! event (function | OpenGameScreen -> Some () | _ -> None)
        |> raiseToOrchestrationWithActions [new Screens.MenuScreen(desktop, updateFn) :> IScreen]

    return! mainLoop desktop updateFn
}

let ScreenJouney updateFn = 
    orchestration {
        let desktop = new Desktop()

        do! event (function | TitleScreenDone -> Some () | _ -> None)
            |> raiseToOrchestrationWithActions [new Screens.StartMenu(desktop, updateFn) :> IScreen] 

        let! fileName = 
            event (function | StartLoadSelected x -> Some x | _ -> None)
            |> raiseToOrchestrationWithActions [new Screens.StartOrLoadMenu(desktop, updateFn) :> IScreen]

        do! mainLoop desktop updateFn

        return fileName
    }
    |> OrchestrationCE.Coordination.collect (function | Break x -> x | _ -> [])
  