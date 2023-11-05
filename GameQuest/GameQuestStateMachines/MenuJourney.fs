module MenuJourney

open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open Utilities
open Myra.Graphics2D.UI

let rec loadLoop desktop updateFn = orchestration {
    let! doLoad = 
        event (function | StartLoadSelected x -> Some x.DoLoad | _ -> None)
        |> raiseToOrchestrationWithActions [Menus.StartOrLoadMenu(desktop, updateFn) :> IMenu]

    return!
        if (not doLoad)
        then orchestration { return None }
        else orchestration {
            let! loadFileResult =
                event (function | LoadFileResult x -> Some x | _ -> None)
                |> raiseToOrchestrationWithActions [Menus.LoadFileDialog(desktop, updateFn) :> IMenu]

            return!
                if loadFileResult.IsNone
                then loadLoop desktop updateFn
                else orchestration { return loadFileResult }
        }
}

let StartMenuJouney updateFn = 
    orchestration {
        let desktop = new Desktop()

        do! event (function | TitleScreenDone -> Some () | _ -> None)
            |> raiseToOrchestrationWithActions [Menus.StartMenu(desktop, updateFn) :> IMenu] 


        let! doLoad =
            event (function | StartLoadSelected x -> Some x.DoLoad | _ -> None)
            |> raiseToOrchestrationWithActions [Menus.StartOrLoadMenu(desktop, updateFn) :> IMenu]
    
        let! file = loadLoop desktop updateFn

        return file
    }
    |> OrchestrationCE.Coordination.collect (function | Break x -> x | _ -> [])
  