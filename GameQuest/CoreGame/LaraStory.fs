module LaraStory

open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open Utilities
open StoryShared

type LaraInternalEvent = 
    | Recruited
    | Introduced
    | Continue
    | ReachesThirdCrystal of Crystal

type LaraExternalEvent = 
    | ArrivedAtCrystal of Crystal

type LaraEvent =
   | InternalEvent of LaraInternalEvent
   | ExternalEvent of LaraExternalEvent

type LaraState =
    { CrystalsFound: Set<Crystal> }

type LaraExposition =
    | Introduce
    | ReachesThirdCrystal of Crystal

    override this.ToString () =
        match this with
        | Introduce -> "Lara, the Fearless Warrior: Lyra, a valiant and fearless warrior, has a tragic history that fuels her determination to protect Elaria. Her combat prowess with various weapons is unmatched."
        | ReachesThirdCrystal Earth -> """Lara: [Lyra approaches the Earth Crystal, her eyes gleaming with determination.] "This is the Earth Crystal, a symbol of strength and resilience. It's a reminder that we can overcome any obstacle, no matter how daunting." """
        | ReachesThirdCrystal Fire -> """Lara: [Lara approaches the Fire Crystal, her gaze steady in the face of the intense flames.] "The Fire Crystal, a beacon of passion and determination. It's a reminder to never give up, no matter how difficult the journey may be." """
        | ReachesThirdCrystal Water -> """Lara: [Lyra approaches the Water Crystal, her footsteps echoing softly in the tranquil chamber.] "The Water Crystal, a symbol of healing and renewal. It's a reminder to take care of ourselves and others, to find peace and balance in our lives." """
        | ReachesThirdCrystal Air -> """Lara: [Lyra stands by the Air Crystal, her hair ruffling in the gentle breeze.] "The Air Crystal, a symbol of freedom and adaptability. It's a reminder to embrace change and go with the flow." """

type LaraInteractive =
    | Recruit
    | Continue

    override this.ToString () =
        match this with
        | Recruit -> "Recruit Lara."
        | Continue -> "Continue."

let laraStory = orchestration {
    do! raiseToOrchestrationWithActions
            [ Interactive (LaraInteractive.Recruit, LaraInternalEvent.Recruited) ]
            (event (function | { Event = InternalEvent Recruited } -> Some () | _ -> None))

    do! raiseToOrchestrationWithActions
            [ Exposition (LaraExposition.Introduce, Some Introduced) ]
            (event (function | { Event = InternalEvent Introduced } -> Some () | _ -> None))

    let! thirdCrystal = 
        raiseToOrchestration
            (event (function | { Event = ExternalEvent (ArrivedAtCrystal crystal); State = { CrystalsFound = crystalsFound } } when crystalsFound.Count = 2 -> Some crystal | _ -> None))

    do! raiseToOrchestrationWithActions
            [ Exposition (LaraExposition.ReachesThirdCrystal thirdCrystal, Some (LaraInternalEvent.ReachesThirdCrystal thirdCrystal)) ]
            (event (function | { Event = InternalEvent (LaraInternalEvent.ReachesThirdCrystal crystal) } when crystal = thirdCrystal -> Some () | _ -> None))

    return 0
}