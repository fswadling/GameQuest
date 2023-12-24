module BattleSystem
open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open OrchestrationCE.EventAndState
open System

type BattleEvent =
    | TeamMemberProgressBarComplete of StoryShared.TeamMember * TimeSpan
    | TeamMemberChosenAction of StoryShared.TeamMember * TeamMemberAction * TimeSpan
    | EnemyProgressBarComplete of TimeSpan
    | EnemyAttack of StoryShared.TeamMember

and TeamMemberAction =
    | Attack
    | UsePotion

module BattleEvent =
    let (|TeamMemberEvent|EnemyEvent|) = function
        | TeamMemberProgressBarComplete (tm, _) -> TeamMemberEvent tm
        | TeamMemberChosenAction (tm, _, _) -> TeamMemberEvent tm
        | EnemyProgressBarComplete _ -> EnemyEvent
        | EnemyAttack tm -> EnemyEvent

type Action<'TActor> =
    | TeamMemberActor of StoryShared.TeamMember * 'TActor
    | EnemyInstant of BattleEvent
    | EnemyActor of 'TActor

module Action =
    let (|Actor|Instant|) = function
        | TeamMemberActor (_, actor) -> Actor actor
        | EnemyInstant battleEvent -> Instant battleEvent
        | EnemyActor actor -> Actor actor

type BattleState =
    { TeamMemberStates: Map<StoryShared.TeamMember, TeamMemberState>
      EnemyState: EnemyState }

    static member Init teamMembers = 
        { TeamMemberStates = teamMembers |> List.map (fun tm -> tm, 100) |> Map.ofList
          EnemyState = 100 }

and TeamMemberState = int
and EnemyState = int

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

let rec teamMemberOrchestration tm progressBarFactory teamMemberActorFactory = orchestration {
    do! raiseToOrchestrationWithActions
            [ TeamMemberActor(tm, progressBarFactory ()) ]
            (event (function | TeamMemberProgressBarComplete _ -> Some () | _ -> None))
            
    do! raiseToOrchestrationWithActions
            [ TeamMemberActor(tm, teamMemberActorFactory ()) ]
            (event (function | TeamMemberChosenAction _ -> Some () | _ -> None))

    return! teamMemberOrchestration tm progressBarFactory teamMemberActorFactory
}

let rec enemyOrchestration progressBarFactory = orchestration {
    let! (battleState: BattleState) = 
        raiseToOrchestrationWithActions
            [ EnemyActor (progressBarFactory ()) ]
            (event (function |{ Event = EnemyProgressBarComplete _; State = state } -> Some state | _ -> None))

    let weakestTeamMember =
        battleState.TeamMemberStates
        |> Map.toSeq
        |> Seq.filter (fun (_, hp) -> hp > 0)
        |> Seq.minBy (fun (_, hp) -> hp)
        |> fst

    do! raiseToOrchestrationWithActions
            [ EnemyInstant (EnemyAttack (weakestTeamMember)) ]
            (event (function | { Event = EnemyAttack _ } -> Some () | _ -> None))

    return! enemyOrchestration progressBarFactory
}

let updateState fullstate = function
    | TeamMemberChosenAction (tm, TeamMemberAction.Attack, _) ->
        let enemyState = fullstate.EnemyState - 10
        { fullstate with EnemyState = enemyState }
    | EnemyAttack tm ->
        BattleState.reduceTeamMemberHealth tm 10 fullstate
    | TeamMemberChosenAction (tm, TeamMemberAction.UsePotion, _) ->
        BattleState.healTeamMember tm fullstate
    | _ -> 
        fullstate

let fullBattleOrchestration initialState tmProgressBarFactory enemyProgressBarFactory teamMemberActorFactory teamMemberDeadActorFactory = 
    let teamMembers = initialState.TeamMemberStates |> Map.keys |> List.ofSeq
    event Some
    |> scan (stateAccumulator updateState) ({ Event = None; State = initialState })
    |> skip 1
    |> compose 
        (event (chooseOrchestrationEventAndStates (function | BattleEvent.TeamMemberEvent (tm) as e -> Some (tm, e) | _ -> None))
        // Combines the orchestrations for each alive team member
        |> compose (
             teamMembers
             |> List.map (
                 fun teamMember -> 
                    event (chooseOrchestrationEventAndStates (fun (tm, e) -> if tm = teamMember then Some e else None))
                    // Only considering alive team members here
                    |> filter (fun { State = state: BattleState } -> BattleState.isTeamMemberAlive teamMember state)
                    |> choose (chooseOrchestrationEvents (Some))
                    |> compose (teamMemberOrchestration teamMember (tmProgressBarFactory teamMember) (teamMemberActorFactory teamMember)))
             |> List.fold combine empty)
        // Any event can lead to a team member death, so handle that possibility here
        |> combine
            (event chooseStateOnGetNextStep
            |> map (fun (state: BattleState) -> 
                state
                |> BattleState.deadTeamMembers
                |> List.map (fun tm -> (tm, (teamMemberDeadActorFactory ())))
                |> List.map TeamMemberActor
                |> Break))
        // Adds the enemy orchestration
        |> combine
            (event (chooseOrchestrationEventAndStates (function | BattleEvent.EnemyEvent as e -> Some e | _ -> None))
            |> map raiseToOptionalEventAndState
            |> compose (enemyOrchestration enemyProgressBarFactory))
        // Recursively apply enemy instant attacks within the state machine immediately after they are generated
        |> combine
            (event (function | { State = state; Event = Some _ } -> Some state | _ -> None)
            |> map (CircuitBreaker.retn)))
    |> applyBreaksRecursively (function | Action.Instant event -> Some event | _ -> None)

type BattleOrchestration<'TActor> = Orchestration<BattleEvent, BattleState, Action<'TActor>>