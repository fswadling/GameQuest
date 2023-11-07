module ScreenJourney

open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open Utilities
open Myra.Graphics2D.UI

let rec loadLoop desktop updateFn = orchestration {
    let! doLoad = 
        event (function | StartLoadSelected x -> Some x.DoLoad | _ -> None)
        |> raiseToOrchestrationWithActions [Screens.StartOrLoadMenu(desktop, updateFn) :> IScreen]

    return!
        if (not doLoad)
        then orchestration { return None }
        else orchestration {
            let! loadFileResult =
                event (function | LoadFileResult x -> Some x | _ -> None)
                |> raiseToOrchestrationWithActions [Screens.LoadFileDialog(desktop, updateFn) :> IScreen]

            return!
                if loadFileResult.IsNone
                then loadLoop desktop updateFn
                else orchestration { return loadFileResult }
        }
}

let rec mainLoop desktop updateFn = orchestration {
    do! event (function | OpenMenuScreen -> Some () | _ -> None)
        |> raiseToOrchestrationWithActions [Screens.GameScreen(desktop, updateFn) :> IScreen]

    do! event (function | OpenGameScreen -> Some () | _ -> None)
        |> raiseToOrchestrationWithActions [Screens.MenuScreen(desktop, updateFn) :> IScreen]

    return! mainLoop desktop updateFn
}

let ScreenJouney updateFn = 
    orchestration {
        let desktop = new Desktop()

        do! event (function | TitleScreenDone -> Some () | _ -> None)
            |> raiseToOrchestrationWithActions [Screens.StartMenu(desktop, updateFn) :> IScreen] 
    
        let! file = loadLoop desktop updateFn

        do! mainLoop desktop updateFn

        return file
    }
    |> OrchestrationCE.Coordination.collect (function | Break x -> x | _ -> [])
  