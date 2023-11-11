module KaiStory

open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open Utilities
open StoryShared

type KaiInternalEvent = 
    | Recruited
    | Introduced
    | ReachesSecondCrystal of Crystal

type KaiExternalEvent = 
    | ArrivedAtCrystal of Crystal

type KaiEvent =
   | InternalEvent of KaiInternalEvent
   | ExternalEvent of KaiExternalEvent

type KaiState =
    { CrystalsFound: Set<Crystal> }

type KaiExposition =
    | Introduce
    | ReachesSecondCrystal of Crystal

    override this.ToString () =
        match this with
        | Introduce -> "Kai, the Rogue with a Heart of Gold: Kai, a nimble and quick-witted rogue, is known for his agile combat skills and keen eye for traps. His past is shrouded in secrets, and he's a master of sneaky tactics."
        | ReachesSecondCrystal Earth -> """Kai: [Kai approaches the Earth Crystal, his keen eyes examining the surroundings.] "Well, would you look at that? We've stumbled upon the Earth Crystal. It's solid, sturdy, just like the land it represents." """
        | ReachesSecondCrystal Fire -> """Kai: [Kai approaches the Fire Crystal, his gaze unwavering in the face of the intense flames.] "Ah, the Fire Crystal! It's a lot like life, isn't it? Unpredictable, powerful, and full of surprises." """
        | ReachesSecondCrystal Water -> """Kai: [Kai approaches the Water Crystal, his footsteps echoing softly in the tranquil chamber.] "Look at this, the Water Crystal. It's like a sanctuary, a place of healing and renewal." """
        | ReachesSecondCrystal Air -> """Kai: [Kai stands by the Air Crystal, his hair ruffling in the gentle breeze.] "We've reached the top, and here's the Air Crystal, as free as the wind. It's about adaptability and going with the flow." """

type KaiInteractive =
    | Recruit
    | Continue

    override this.ToString () =
        match this with
        | Recruit -> "Recruit Kai."
        | Continue -> "Continue."

let kaiStory = orchestration {
    do! raiseToOrchestrationWithActions
            [ Interactive (KaiInteractive.Recruit, KaiInternalEvent.Recruited) ]
            (event (function | { Event = InternalEvent Recruited } -> Some () | _ -> None))

    do! raiseToOrchestrationWithActions
            [ Exposition (KaiExposition.Introduce, Some Introduced) ]
            (event (function | { Event = InternalEvent Introduced } -> Some () | _ -> None))

    let! secondCrystal = 
        raiseToOrchestration
            (event (function | { Event = ExternalEvent (ArrivedAtCrystal crystal); State = { CrystalsFound = crystalsFound } } when crystalsFound.Count = 1 -> Some crystal | _ -> None))

    do! raiseToOrchestrationWithActions
            [ Exposition (KaiExposition.ReachesSecondCrystal secondCrystal, Some (KaiInternalEvent.ReachesSecondCrystal secondCrystal)) ]
            (event (function | { Event = InternalEvent (KaiInternalEvent.ReachesSecondCrystal crystal) } when crystal = secondCrystal -> Some () | _ -> None))

    return 0
}