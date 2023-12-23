module BattleSystem
open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open OrchestrationCE.EventAndState
open System

type BattleInteraction<'TInstant, 'TActor> = 
    | Actor of 'TActor
    | Instant of 'TInstant

module BattleInteraction =
    let mapInstant f = function
        | Actor a -> Actor a
        | Instant i -> Instant (f i)

type TeamMemberAction =
    | Attack
    | UsePotion

type TeamMemberEvent =
   | ProgressBarComplete of TimeSpan
   | ChosenAction of TeamMemberAction * TimeSpan

type EnemyEvent = 
    | EnemyProgressBarComplete of TimeSpan
    | EnemyAttack of StoryShared.TeamMember

type BattleEvent =
    | TeamMemberEvent of StoryShared.TeamMember * TeamMemberEvent
    | EnemyEvent of EnemyEvent

type TeamMemberState = int
type EnemyState = int

type FullBattleInteractive<'TInstant, 'TActor> =
    | TeamMemberInteraction of StoryShared.TeamMember * 'TActor
    | EnemyInteraction of BattleInteraction<'TInstant, 'TActor>

type BattleState =
    { TeamMemberStates: Map<StoryShared.TeamMember, TeamMemberState>
      EnemyState: EnemyState }

    static member Init teamMembers = 
        { TeamMemberStates = teamMembers |> List.map (fun tm -> tm, 100) |> Map.ofList
          EnemyState = 100 }

module BattleState =
    let (|Victory|Defeat|Ongoing|) state = 
        if (state.TeamMemberStates |> Map.forall (fun _ hp -> hp <= 0)) then
            Defeat
        else if (state.EnemyState <= 0) then
            Victory
        else 
            Ongoing

    let isTeamMemberAlive tm state =
        state.TeamMemberStates |> Map.exists (fun tm2 hp -> tm = tm2 && hp > 0)

    let deadTeamMembers state = 
        state.TeamMemberStates |> Map.filter (fun _ hp -> hp <= 0) |> Map.keys |> List.ofSeq

    let reduceTeamMemberHealth tm damage state =
        let teamMemberStates = 
            state.TeamMemberStates
            |> Map.map (fun tm' health -> 
                if tm' = tm 
                then Math.Max(health - damage, 0)
                else health)
        { state with TeamMemberStates = teamMemberStates }

    let healTeamMember tm state =
        let teamMemberStates = 
            state.TeamMemberStates
            |> Map.map (fun tm' health -> 
                if tm' = tm 
                then Math.Min(health + 50, 100)
                else health)
        { state with TeamMemberStates = teamMemberStates }

let rec teamMemberOrchestration progressBarFactory teamMemberActorFactory = orchestration {
    do! raiseToOrchestrationWithActions
            [ progressBarFactory () ]
            (event (function | TeamMemberEvent.ProgressBarComplete _ -> Some () | _ -> None))
            
    do! raiseToOrchestrationWithActions
            [ teamMemberActorFactory () ]
            (event (function | ChosenAction _ -> Some () | _ -> None))

    return! teamMemberOrchestration progressBarFactory teamMemberActorFactory
}

let rec enemyOrchestration progressBarFactory = orchestration {
    let! (battleState: BattleState) = 
        raiseToOrchestrationWithActions
            [ Actor (progressBarFactory ()) ]
            (event (function |{ Event = EnemyEvent.EnemyProgressBarComplete _; State = state } -> Some state | _ -> None))

    let weakestTeamMember =
        battleState.TeamMemberStates
        |> Map.toSeq
        |> Seq.filter (fun (_, hp) -> hp > 0)
        |> Seq.minBy (fun (_, hp) -> hp)
        |> fst

    do! raiseToOrchestrationWithActions
            [ Instant (EnemyAttack (weakestTeamMember)) ]
            (event (function | { Event = EnemyAttack _ } -> Some () | _ -> None))

    return! enemyOrchestration progressBarFactory
}

let updateState fullstate = function
    | TeamMemberEvent (tm, ChosenAction (TeamMemberAction.Attack, _)) ->
        let enemyState = fullstate.EnemyState - 10
        { fullstate with EnemyState = enemyState }
    | EnemyEvent (EnemyAttack tm) ->
        fullstate 
        |> BattleState.reduceTeamMemberHealth tm 10
    | TeamMemberEvent (tm, ChosenAction (TeamMemberAction.UsePotion, _)) ->
        fullstate 
        |> BattleState.healTeamMember tm
    | _ -> 
        fullstate

let fullBattleOrchestration initialState tmProgressBarFactory enemyProgressBarFactory teamMemberActorFactory teamMemberDeadActorFactory = 
    let teamMembers = initialState.TeamMemberStates |> Map.keys |> List.ofSeq
    event Some
    |> scan (stateAccumulator updateState) ({ Event = None; State = initialState })
    |> skip 1
    |> compose 
        ((event (chooseOrchestrationEventAndStates (function | TeamMemberEvent (tm, e) -> Some (tm, e) | _ -> None))
        // Combines the orchestrations for each alive team member
        |> compose (
             teamMembers
             |> List.map (
                 fun teamMember -> 
                    event (chooseOrchestrationEventAndStates (fun (tm, e) -> if tm = teamMember then Some e else None))
                    // Only considering alive team members here
                    |> filter (fun { State = state: BattleState } -> BattleState.isTeamMemberAlive teamMember state)
                    |> choose (chooseOrchestrationEvents (Some))
                    |> compose (teamMemberOrchestration (tmProgressBarFactory teamMember) (teamMemberActorFactory teamMember))
                    |> map (CircuitBreaker.mapBreak (List.map (fun y -> (teamMember, y)))))
             |> List.fold combine empty
             |> mapBreak (fun (tm, interaction) -> TeamMemberInteraction (tm, interaction)))
        // Any event can lead to a team member death, so handle that possibility here
        |> combine
            (event chooseStateOnGetNextStep
            |> map (fun (state: BattleState) -> 
                state
                |> BattleState.deadTeamMembers
                |> List.map (fun tm -> TeamMemberInteraction (tm, (teamMemberDeadActorFactory ())))
                |> Break))
        // Adds the enemy orchestration
        |> combine
            (event (chooseOrchestrationEventAndStates (function | EnemyEvent e -> Some e | _ -> None))
            |> map raiseToOptionalEventAndState
            |> compose (enemyOrchestration enemyProgressBarFactory)
            |> mapBreak ((BattleInteraction.mapInstant EnemyEvent) >> EnemyInteraction)))
        // Recursively apply enemy instant attacks within the state machine immediately after they are generated
        |> combine
            (event (function | { State = state; Event = Some _ } -> Some state | _ -> None)
            |> map (CircuitBreaker.retn)))
    |> applyBreaksRecursively (function | EnemyInteraction (Instant instant) -> Some instant | _ -> None)

type BattleOrchestration<'TActor> = Orchestration<BattleEvent, BattleState, FullBattleInteractive<BattleEvent, 'TActor>>