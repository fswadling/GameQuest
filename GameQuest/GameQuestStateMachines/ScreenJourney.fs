module ScreenJourney

open Myra.Graphics2D.UI
open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open Screens
open GameState

let rec mainLoop desktop updateFn gameState = orchestration {
    let! gameState =
        event (function | OpenMenuScreen state -> Some state | _ -> None)
        |> raiseToOrchestrationWithActions [new Screens.GameScreen(desktop, updateFn, gameState) :> IScreen]

    let! gameState =
        event (function | OpenGameScreen state -> Some state | _ -> None)
        |> raiseToOrchestrationWithActions [new Screens.MenuScreen(desktop, updateFn, gameState, Story.story) :> IScreen]

    return! mainLoop desktop updateFn gameState
}

let ScreenJouney updateFn = 
    orchestration {
        let desktop = new Desktop()

        do! event (function | TitleScreenDone -> Some () | _ -> None)
            |> raiseToOrchestrationWithActions [new Screens.StartMenu(desktop, updateFn) :> IScreen] 

        let! fileName = 
            event (function | StartLoadSelected x -> Some x | _ -> None)
            |> raiseToOrchestrationWithActions [new Screens.StartOrLoadMenu(desktop, updateFn) :> IScreen]

        let gameState: GameState.GameState option = 
            match fileName with
            | Some fileName -> GameState.Load(fileName, Story.story)
            | None -> Some (GameState.New(Story.story))

        do! 
            match gameState with
            | Some gameState -> mainLoop desktop updateFn gameState
            | None -> orchestration { return () }

        return ()
    }
    |> OrchestrationCE.Coordination.collect (function | Break x -> x | _ -> [])
  