module ScreenJourney

open Myra.Graphics2D.UI
open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open Screens
open BattleScreen
open GameState

let rec mainLoop desktop updateFn gameState = orchestration {
    let! screenEvent =
        event (function 
            | OpenMenuScreen state -> Some (OpenMenuScreen state)
            | OpenBattleScreen (battleState, gameState) -> Some (OpenBattleScreen (battleState, gameState))
            | _ -> None)
        |> raiseToOrchestrationWithActions [new Screens.GameScreen(desktop, updateFn, gameState) :> IScreen]

    let! gameState =
        match screenEvent with
        | OpenMenuScreen gameState ->
            event (function | OpenGameScreen state -> Some state | _ -> None)
            |> raiseToOrchestrationWithActions [new Screens.MenuScreen(desktop, updateFn, gameState, Story.story) :> IScreen]
        | OpenBattleScreen (battleState, gameState) ->
            event (function | OpenGameScreen state -> Some state | _ -> None)
            |> raiseToOrchestrationWithActions [new BattleScreen(desktop, updateFn, battleState, gameState) :> IScreen]
        | _ -> failwith "Unexpected screen event"

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
  