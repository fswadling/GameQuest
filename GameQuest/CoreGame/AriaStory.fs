module AriaStory

open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open OrchestrationCE.EventAndState
open Utilities
open StoryShared

type AriaInternalEvent = 
    | Recruited
    | Introduced
    | ReachesFirstCrystal of Crystal

type AriaExternalEvent = 
    | ArrivedAtCrystal of Crystal

type AriaEvent =
   | InternalEvent of AriaInternalEvent
   | ExternalEvent of AriaExternalEvent

type AriaState =
    { CrystalsFound: Set<Crystal> }

type AriaExposition =
    | Introduce
    | ReachesFirstCrystal of Crystal

    override this.ToString () =
        match this with
        | Introduce -> "Aria, the Enigmatic Mage: Aria, with her mysterious past and deep knowledge of Elaria's magic, is your magical prodigy. Her spells will be essential to unlocking the crystals' secrets"
        | ReachesFirstCrystal Fire -> """Aria: [Aria stands before the fiery crystal, her eyes shining with a mixture of fascination and reverence.] "Behold, the Fire Crystal! Its flames dance with a spirit of passion and power. It represents the unyielding force of transformation and the intensity of the soul." """
        | ReachesFirstCrystal Water -> """Aria: [Aria stands before the crystalline waters, her eyes shining with a mixture of fascination and reverence.] "Behold, the Water Crystal! Its waters flow with a spirit of tranquility and harmony. It represents the fluidity of life and the purity of the soul." """
        | ReachesFirstCrystal Earth -> """Aria: [Aria stands before the earthen crystal, her eyes shining with a mixture of fascination and reverence.] "Behold, the Earth Crystal! Its soil is rich with a spirit of growth and vitality. It represents the strength of the body and the bounty of the soul." """
        | ReachesFirstCrystal Air -> """Aria: [Aria stands before the swirling winds, her eyes shining with a mixture of fascination and reverence.] "Behold, the Air Crystal! Its winds blow with a spirit of freedom and adventure. It represents the boundless potential of the soul." """

type AriaInteractive =
    | Recruit
    | Continue

    override this.ToString () =
        match this with
        | Recruit -> "Recruit Aria."
        | Continue -> "Continue."

let ariaStory = orchestration {
    do! raiseToOrchestrationWithActions
            [ Interactive (AriaInteractive.Recruit, AriaInternalEvent.Recruited) ]
            (event (function | { Event = InternalEvent Recruited } -> Some () | _ -> None))

    do! raiseToOrchestrationWithActions
            [ Exposition (AriaExposition.Introduce, Some Introduced) ]
            (event (function | { Event = InternalEvent Introduced } -> Some () | _ -> None))

    let! firstCrystal = 
        raiseToOrchestration
            (event (function | { Event = ExternalEvent (ArrivedAtCrystal crystal); State = { CrystalsFound = crystalsFound } } when crystalsFound.IsEmpty -> Some crystal | _ -> None))

    do! raiseToOrchestrationWithActions
            [ Exposition (AriaExposition.ReachesFirstCrystal firstCrystal, Some (AriaInternalEvent.ReachesFirstCrystal firstCrystal)) ]
            (event (function | { Event = InternalEvent (AriaInternalEvent.ReachesFirstCrystal crystal) } when crystal = firstCrystal -> Some () | _ -> None))

    return 0
}