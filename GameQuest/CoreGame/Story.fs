module Story

open OrchestrationCE
open Coordination
open Orchestration
open Utilities
open StoryShared

open AriaStory
open KaiStory
open LaraStory

type StoryEvent = 
    | AdventureBegins
    | CalamityOccurs
    | BattleWon
    | AriaEvent of AriaInternalEvent
    | KaiEvent of KaiInternalEvent
    | LaraEvent of LaraInternalEvent
    | ArrivedAtCrystal of Crystal
    | CrystalFound of Crystal
    | TheEnd

type Exposition =
    | AdventureBegins
    | CalamityOccurs
    | AdventureEnds
    | StandBeforeCrystal of Crystal
    | AriaExposition of AriaExposition
    | KaiExposition of KaiExposition
    | LaraExposition of LaraExposition

    override this.ToString () =
        match this with
        | AdventureBegins -> "In the enchanting realm of Elaria, where magic courses through every living thing, a long-forgotten prophecy resurfaces, casting a shadow over the land. The prophecy foretells the existence of four mighty elemental crystals – Earth, Fire, Water, and Air – said to hold the power to shape the world itself."
        | CalamityOccurs -> "You, a humble village resident, find your world turned upside down when an unforeseen disaster shatters the ancient Elemental Seal, throwing Elaria into turmoil. As fate's chosen one, your destiny is to embark on a perilous quest to locate the elemental crystals and prevent the impending catastrophe."
        | AdventureEnds -> "With the power of the crystals you defeat the evil sorceror Malachar and save the world!."
        | StandBeforeCrystal crystal -> $"You stand before the {crystal} crystal."
        | AriaExposition e -> e.ToString ()
        | KaiExposition e -> e.ToString ()
        | LaraExposition e -> e.ToString ()

type Interaction =
    | Continue
    | GoToCrystal of Crystal
    | TakeCrystal of Crystal
    | AriaInteraction of AriaInteractive
    | KaiInteraction of KaiInteractive
    | LaraInteraction of LaraInteractive

    override this.ToString () =
        match this with
        | Continue -> "Continue"
        | GoToCrystal crystal -> $"Go to {crystal} crystal."
        | TakeCrystal crystal -> $"Take {crystal} crystal."
        | AriaInteraction i -> i.ToString ()
        | KaiInteraction i -> i.ToString ()
        | LaraInteraction i -> i.ToString ()

type State =
    { CompanionsRecruited: Set<TeamMember>
      CrystalsFound: Set<Crystal> }

module State =
    let init = { CompanionsRecruited = (Set.singleton You); CrystalsFound = Set.empty }

    let updateState state = function
        | CrystalFound crystal -> { state with State.CrystalsFound = Set.add crystal state.CrystalsFound }
        | AriaEvent AriaInternalEvent.Recruited -> { state with State.CompanionsRecruited = Set.add Aria state.CompanionsRecruited }
        | KaiEvent KaiInternalEvent.Recruited -> { state with State.CompanionsRecruited = Set.add Kai state.CompanionsRecruited }
        | LaraEvent LaraInternalEvent.Recruited -> { state with State.CompanionsRecruited = Set.add Lara state.CompanionsRecruited }
        | _ -> state

type MainAction<'TEvent, 'TExposition, 'TInteractive, 'TBattleState> =
    | Exposition of 'TExposition * ('TEvent option)
    | Interactive of 'TInteractive * 'TEvent
    | Battle of 'TBattleState

module MainAction =
    let fromAction = function
        | Action.Exposition (exposition, event) -> Exposition (exposition, event)
        | Action.Interactive (interactive, event) -> Interactive (interactive, event)      

type Story = Coordination<StoryEvent option, MainAction<StoryEvent, Exposition, Interaction, State>>

let getAriaState { State.CrystalsFound = crystalsFound } =
   { AriaState.CrystalsFound = crystalsFound }

let chooseAriaEvent = function
    | Some { Event = ArrivedAtCrystal crystal; State = state } -> 
        Some (Some { Event = AriaEvent.ExternalEvent (AriaExternalEvent.ArrivedAtCrystal crystal); State = getAriaState state }) 
    | Some { Event = AriaEvent e; State = state } -> 
        Some (Some { Event = AriaEvent.InternalEvent e; State = getAriaState state })
    | None -> Some None
    | _ -> None

let aria =
    event chooseAriaEvent
    |> Coordination.compose ariaStory
    |> Orchestration.mapBreak 
        ((Action.mapEvent AriaEvent) 
        >> (Action.mapExposition AriaExposition) 
        >> (Action.mapInteractive AriaInteraction)
        >> MainAction.fromAction)

let getKaiState { State.CrystalsFound = crystalsFound } =
   { KaiState.CrystalsFound = crystalsFound }

let chooseKaiEvent = function
    | Some { Event = ArrivedAtCrystal crystal; State = state } -> 
        Some (Some { Event = KaiEvent.ExternalEvent (KaiExternalEvent.ArrivedAtCrystal crystal); State = getKaiState state }) 
    | Some { Event = KaiEvent e; State = state } -> 
        Some (Some { Event = KaiEvent.InternalEvent e; State = getKaiState state })
    | None -> Some None
    | _ -> None

let kai =
    event chooseKaiEvent
    |> Coordination.compose kaiStory
    |> Orchestration.mapBreak 
        ((Action.mapEvent KaiEvent) 
        >> (Action.mapExposition KaiExposition) 
        >> (Action.mapInteractive KaiInteraction)
        >> MainAction.fromAction)

let getLaraState { State.CrystalsFound = crystalsFound } =
   { LaraState.CrystalsFound = crystalsFound }

let chooseLaraEvent = function
    | Some { Event = ArrivedAtCrystal crystal; State = state } -> 
        Some (Some { Event = LaraEvent.ExternalEvent (LaraExternalEvent.ArrivedAtCrystal crystal); State = getLaraState state })
    | Some { Event = LaraEvent e; State = state } -> 
        Some (Some { Event = LaraEvent.InternalEvent e; State = getLaraState state })
    | None -> Some None
    | _ -> None

let lara =
    event chooseLaraEvent
    |> Coordination.compose laraStory
    |> Orchestration.mapBreak 
        ((Action.mapEvent LaraEvent) 
        >> (Action.mapExposition LaraExposition) 
        >> (Action.mapInteractive LaraInteraction)
        >> MainAction.fromAction)

let rec crystalQuest (crystalsRemaining: Set<Crystal>) =
    orchestration {
        if (crystalsRemaining.IsEmpty) then
            ()
        else

        let goToCrystalActions =
            crystalsRemaining
            |> Seq.map (fun crystal -> Interactive (GoToCrystal crystal, ArrivedAtCrystal crystal))
            |> Seq.toList

        let! chosenCrystal = 
           raiseToOrchestrationWithActions
                goToCrystalActions
                (event (function | { Event = ArrivedAtCrystal crystal } -> Some crystal | _ -> None))

        do! raiseToOrchestrationWithActions
                [ Exposition (Exposition.StandBeforeCrystal chosenCrystal, None)
                  Interactive (TakeCrystal chosenCrystal, CrystalFound chosenCrystal) ]
                (event (function | { Event = CrystalFound crystal } when crystal = chosenCrystal -> Some () | _ -> None))

        let remainingCrystals = Set.remove chosenCrystal crystalsRemaining 

        do! crystalQuest remainingCrystals

        return ()
    }

let mainQuest = 
    orchestration {
        do! raiseToOrchestrationWithActions
                [ Exposition (Exposition.AdventureBegins, None)
                  Interactive (Interaction.Continue, StoryEvent.AdventureBegins) ]
                (event (function | { Event = StoryEvent.AdventureBegins } -> Some () | _ -> None))

        let! state =
            raiseToOrchestrationWithActions
                [ Exposition (Exposition.CalamityOccurs, None)
                  Interactive (Interaction.Continue, StoryEvent.CalamityOccurs) ]
                (event (function | { Event = StoryEvent.CalamityOccurs; State = state } -> Some state | _ -> None))

        do! raiseToOrchestrationWithActions
                [ Battle state]
                (event (function | { Event = StoryEvent.BattleWon } -> Some () | _ -> None))

        yield aria
        yield kai
        yield lara

        do! raiseToOrchestration
                (event (function | { State = { State.CompanionsRecruited = companionsRecruited } } when companionsRecruited.Count = 4 -> Some () | _ -> None))

        do! crystalQuest (Set.ofList [ Earth; Fire; Water; Air ])

        let! state = 
            raiseToOrchestration
                (event (function | { State = state } -> Some state))

        do! raiseToOrchestrationWithActions
                [ Battle state]
                (event (function | { Event = StoryEvent.BattleWon } -> Some () | _ -> None))

        do! raiseToOrchestrationWithActions
                [ Exposition (Exposition.AdventureEnds, Some TheEnd) ]
                (event (function | { Event = StoryEvent.TheEnd } -> Some () | _ -> None)) 

        return orchestration { return 0 }
    }
    |> Orchestration.mergeMap id

let story = 
    baseStoryWithState State.updateState State.init
    |> Coordination.compose mainQuest
    |> Coordination.collect (function | Break x -> x | _ -> [])