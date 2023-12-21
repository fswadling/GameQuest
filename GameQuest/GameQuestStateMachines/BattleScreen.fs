module BattleScreen

open System
open Myra.Graphics2D.UI
open Microsoft.Xna.Framework
open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open OrchestrationCE.EventAndState
open Screens
open GameState

type IActor =
    abstract member OnUpdate: gameTime: GameTime -> unit
    abstract member GetPanel: unit -> VerticalStackPanel

type BattleInteraction<'TInstant> = 
    | Actor of IActor
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

type BattleOverState =
    | Victory
    | Defeat

type ProgressBarActor (notifyComplete) =
    let progresBar = HorizontalProgressBar(Width=100)
    let mutable startTime: TimeSpan option = None
    interface IActor with
        member this.OnUpdate gameTime =
            match startTime with
            | None ->
                startTime <- Some gameTime.TotalGameTime
            | Some startTime ->
                let progress = (gameTime.TotalGameTime - startTime) / TimeSpan.FromSeconds(2.0)
                if (progress < 1.0) then
                    progresBar.Value <- (float32)progress * 100.0f
                else
                    notifyComplete gameTime.TotalGameTime

        member this.GetPanel () =
            let panel = VerticalStackPanel()
            do panel.Widgets.Add(progresBar)
            panel

type TeamMemberActor (onActionChosen) =
    let mutable gameTime = TimeSpan.Zero
    let panel = VerticalStackPanel()
    let attackButton = Button(Content = Label(Text = "Attack"))
    do attackButton.Click.Add(fun _ -> onActionChosen (Attack, gameTime))
    let usePotionButton = Button(Content = Label(Text = "Use Potion"))
    do usePotionButton.Click.Add(fun _ -> onActionChosen (UsePotion, gameTime))
   
    do panel.Widgets.Add(attackButton)
    do panel.Widgets.Add(usePotionButton)

    interface IActor with
        member this.OnUpdate gt =
            gameTime <- gt.TotalGameTime
            ()

        member this.GetPanel () = 
            panel

type TeamMemberDeadActor () =
    let panel = VerticalStackPanel()
    let label = Label(Text = "Dead")
    do panel.Widgets.Add(label)

    interface IActor with
        member this.OnUpdate gt = ()

        member this.GetPanel () = 
            panel

let rec teamMemberOrchestration progressBarFactory teamMemberActorFactory = orchestration {
    let! progressCompleteTime = 
        raiseToOrchestrationWithActions
            [ Actor (progressBarFactory ()) ]
            (event (function | TeamMemberEvent.ProgressBarComplete gameTime -> Some gameTime | _ -> None))
            
    let! chosenAction, gameTime =
        raiseToOrchestrationWithActions
            [ Actor (teamMemberActorFactory ()) ]
            (event (function | ChosenAction (action, time) -> Some (action, time) | _ -> None))

    return! teamMemberOrchestration progressBarFactory teamMemberActorFactory
}

let rec enemyOrchestration progressBarFactory = orchestration {
    let! progressCompleteTime = 
        raiseToOrchestrationWithActions
            [ Actor (progressBarFactory ()) ]
            (event (function | EnemyEvent.EnemyProgressBarComplete gameTime -> Some gameTime | _ -> None))

    do! raiseToOrchestrationWithActions
            [ Instant (EnemyAttack (StoryShared.TeamMember.You)) ]
            (event (function | EnemyAttack tm -> Some () | _ -> None))

    return! enemyOrchestration progressBarFactory
}

type FullBattleState =
    { TeamMemberStates: Map<StoryShared.TeamMember, TeamMemberState>
      EnemyState: EnemyState }

    static member Init teamMembers = 
        { TeamMemberStates = teamMembers |> List.map (fun tm -> tm, 100) |> Map.ofList
          EnemyState = 100 }

    member this.BattleOverState = 
        if (this.TeamMemberStates |> Map.forall (fun _ hp -> hp <= 0)) then
            Some Defeat
        else if (this.EnemyState <= 0) then
            Some Victory
        else
            None

    member this.IsTeamMemberAlive tm =
        this.TeamMemberStates |> Map.exists (fun tm2 hp -> tm = tm2 && hp > 0)

    member this.DeadTeamMembers =
        this.TeamMemberStates |> Map.filter (fun _ hp -> hp <= 0) |> Map.keys |> List.ofSeq

    member this.ReduceTeamMemberHealth tm damage =
        let teamMemberStates = 
            this.TeamMemberStates
            |> Map.map (fun tm' health -> 
                if tm' = tm 
                then Math.Max(health - damage, 0)
                else health)
        { this with TeamMemberStates = teamMemberStates }

    member this.HealTeamMember tm =
        let teamMemberStates = 
            this.TeamMemberStates
            |> Map.map (fun tm' health -> 
                if tm' = tm 
                then Math.Min(health + 50, 100)
                else health)
        { this with TeamMemberStates = teamMemberStates }

type FullBattleInteractive<'a> =
    | TeamMemberInteraction of StoryShared.TeamMember * BattleInteraction<'a>
    | EnemyInteraction of BattleInteraction<'a>

let updateState fullstate = function
    | TeamMemberEvent (tm, ChosenAction (TeamMemberAction.Attack, _)) ->
        let enemyState = fullstate.EnemyState - 10
        { fullstate with EnemyState = enemyState }
    | EnemyEvent (EnemyAttack tm) ->
        fullstate.ReduceTeamMemberHealth tm 10
    | TeamMemberEvent (tm, ChosenAction (TeamMemberAction.UsePotion, _)) ->
        fullstate.HealTeamMember tm
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
                    |> filter (fun { State = state: FullBattleState } -> state.IsTeamMemberAlive(teamMember))
                    |> choose (chooseOrchestrationEvents (Some))
                    |> compose (teamMemberOrchestration (tmProgressBarFactory teamMember) (teamMemberActorFactory teamMember))
                    |> map (CircuitBreaker.mapBreak (List.map (fun y -> teamMember, y))))
             |> List.fold combine empty
             |> mapBreak (fun (tm, interaction) -> TeamMemberInteraction (tm, BattleInteraction.mapInstant TeamMemberEvent interaction)))
        // Adds the enemy orchestration
        |> combine
            (event (chooseOrchestrationEvents (function | EnemyEvent e -> Some e | _ -> None))
            |> compose (enemyOrchestration enemyProgressBarFactory)
            |> mapBreak ((BattleInteraction.mapInstant EnemyEvent) >> EnemyInteraction))
        // Any event can lead to a team member death, so handle that possibility here
        |> combine
            (event chooseStateOnGetNextStep
            |> map (fun (state: FullBattleState) -> 
                state.DeadTeamMembers
                |> List.map (fun tm -> TeamMemberInteraction (tm, (Actor (teamMemberDeadActorFactory ()))))
                |> Break))
        // Return the current state when applying an event
        |> combine
            (event (function | { State = state; Event = Some _ } -> Some state | _ -> None)
            |> map (CircuitBreaker.retn))))

type BattleOrchestration = Orchestration<BattleEvent, FullBattleState, FullBattleInteractive<BattleEvent>>

type BattleState (battle: BattleOrchestration, state, winBattle, loseBattle) =
    let results = lazy (
        let { CoordinationResult.Result = results } = battle None
        results
    )

    let interactives = 
        lazy (
            results.Value
            |> List.choose (function | Break interactives -> Some interactives | _ -> None)
            |> List.collect id
        )

    let allActors = 
        lazy (
            interactives.Value
            |> List.choose 
                (function 
                    | TeamMemberInteraction (_, Actor actor) 
                    | EnemyInteraction (Actor actor) -> Some actor | _ -> None)
            |> List.toArray
        )

    member this.BattleState with get() =
        state

    member this.AllEnemyInstants with get() =
        interactives.Value
        |> List.choose (function | EnemyInteraction (Instant event) -> Some event | _ -> None)

    member this.TeamMemberActors with get() =
        interactives.Value
        |> List.choose (function | TeamMemberInteraction (tm, Actor actor) -> Some (tm, actor) | _ -> None)
        |> dict
        |> System.Collections.Generic.Dictionary

    member this.EnemyActor with get() =
        interactives.Value
        |> List.choose (function | EnemyInteraction (Actor actor) -> Some actor | _ -> None)
        |> List.tryLast

    member this.InstantActions with get() =
        interactives.Value
        |> List.choose (function | TeamMemberInteraction (_, Instant action) -> Some action | _ -> None)

    member this.DoEvent (event: BattleEvent) =
        let { Result = result; Next = next } = battle (Some event)
        let newState = 
            result 
            |> List.choose (function | Continue state -> Some state | _ -> None) 
            |> List.head

        if newState.BattleOverState.IsSome then
            match newState.BattleOverState.Value with
            | Victory -> winBattle ()
            | Defeat -> loseBattle ()
            None
        else

        let nextState = Option.map (fun x -> BattleState(x, newState, winBattle, loseBattle)) next

        let nextState = 
            match nextState with
            | None -> None
            | Some nextState -> 
                let instantResult = 
                    List.tryHead nextState.AllEnemyInstants
                    |> Option.map (fun y -> nextState.DoEvent(y))
                match instantResult with
                // If no instant events, then just return the next state
                | None -> Some nextState
                // If there is an instant event, then return the state result of applying it
                | Some (Some nextState) -> Some nextState
                // If there was an instant event, but it didn't result in a state, then return None
                | Some None -> None

        nextState

    member this.OnUpdate gameTime =
        for (actor: IActor) in allActors.Value
            do actor.OnUpdate(gameTime)

[<AllowNullLiteral>]
type Beligerant (name: string, actor: IActor) =
    let memberPanel = VerticalStackPanel()
    do memberPanel.Widgets.Add(Label(Text = name))
    let healthLabel = Label(Text="Health")
    do memberPanel.Widgets.Add(healthLabel)
    let actorPanel = VerticalStackPanel()
    do actorPanel.Widgets.Add(actor.GetPanel())
    do memberPanel.Widgets.Add(actorPanel)

    member this.MemberPanel with get() = memberPanel

    member this.UpdateHealth(health: float) =
       healthLabel.Text <- sprintf "Health: %f" health

    member this.UpdateActorPanel(actor: IActor) =
        actorPanel.Widgets.Clear()
        actorPanel.Widgets.Add(actor.GetPanel())

    static member GetTeamBeligerants (battleState: BattleState) =
        battleState.TeamMemberActors
        |> Seq.map (fun kvp -> kvp.Key, Beligerant(kvp.Key.ToString(), kvp.Value))
        |> dict
        |> System.Collections.Generic.Dictionary

    static member GetEnemyBeligerant (battleState: BattleState) =
        battleState.EnemyActor
        |> Option.map (fun actor -> Beligerant("Enemy", actor))

type BattleScreen (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>, storyState: Story.State, gameState: GameState) =
    let mutable battleState: BattleState option = None
    let mutable team = System.Collections.Generic.Dictionary<StoryShared.TeamMember, Beligerant>()
    let mutable enemy: Beligerant = null

    let updateBeligerants () =
        let tmActors = battleState.Value.TeamMemberActors
        let tmState = battleState.Value.BattleState.TeamMemberStates

        for kvp in team do
            let beligerant = kvp.Value
            let actor = tmActors[kvp.Key]
            do beligerant.UpdateActorPanel(actor)
            let tmState = tmState.TryFind kvp.Key
            do beligerant.UpdateHealth tmState.Value

        let enemyActor = battleState.Value.EnemyActor
        do enemy.UpdateActorPanel(enemyActor.Value);
        do enemy.UpdateHealth(battleState.Value.BattleState.EnemyState)

    let applyEventAndUpdate event =
        let newBattleState = 
            Option.bind (fun (x: BattleState) -> x.DoEvent(event)) battleState

        if newBattleState.IsNone then
            ()
        else

        battleState <- newBattleState
        updateBeligerants()

    let tmProgressBarComplete teamMember gameTime =
        do TeamMemberEvent (teamMember, TeamMemberEvent.ProgressBarComplete(gameTime))
           |> applyEventAndUpdate

    let enemyProgressBarComplete gameTime =
        do EnemyEvent (EnemyEvent.EnemyProgressBarComplete(gameTime))
           |> applyEventAndUpdate

    let tmActionChosen teamMember (action, gameTime) =
        do TeamMemberEvent (teamMember, TeamMemberEvent.ChosenAction(action, gameTime))
            |> applyEventAndUpdate

    let progressBarActorFactory teamMember () = 
        new ProgressBarActor(tmProgressBarComplete teamMember)
        :> IActor

    let enemyProgressBarActorFactory () =
        new ProgressBarActor(enemyProgressBarComplete)
        :> IActor

    let teamMemberActorFactory teamMember () =
        new TeamMemberActor(tmActionChosen teamMember)
        :> IActor

    let teamMemberDeadActorFactory () =
        new TeamMemberDeadActor()
        :> IActor

    let battleOrchestration = 
        fullBattleOrchestration
            (FullBattleState.Init (storyState.CompanionsRecruited |> Set.toList))
            progressBarActorFactory
            enemyProgressBarActorFactory
            teamMemberActorFactory
            teamMemberDeadActorFactory

    let winBattle () =
        let newState = gameState.DoEvent (Story.StoryEvent.BattleOver)
        match newState with
        | Some newState -> updateScreenFn.Invoke(OpenGameScreen newState)
        | None -> ()

    let loseBattle () = 
         do updateScreenFn.Invoke(OpenGameOverScreen gameState)

    let getRoot () =
        let panel = Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = VerticalStackPanel()
        let winButtonLabel = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Win");
        let winButton = Button(Content = winButtonLabel)
        let teamStack = HorizontalStackPanel(VerticalAlignment = VerticalAlignment.Center)

        for kvp in team do
            do teamStack.Widgets.Add(kvp.Value.MemberPanel)

        let enemyStack = HorizontalStackPanel(VerticalAlignment = VerticalAlignment.Center)
        do enemyStack.Widgets.Add(enemy.MemberPanel)

        winButton.TouchDown.Add(fun _ -> winBattle ())
        stack.Widgets.Add(winButton);
        stack.Widgets.Add(enemyStack);
        stack.Widgets.Add(teamStack);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Dispose(): unit = 
            ()

        member this.Initialise () =
            let state = FullBattleState.Init (storyState.CompanionsRecruited |> Set.toList)
            let bs = BattleState(battleOrchestration, state, winBattle, loseBattle)
            do battleState <- Some bs
            do team <- Beligerant.GetTeamBeligerants bs
            do enemy <- (Beligerant.GetEnemyBeligerant bs).Value
            do desktop.Root <- getRoot ()
            do updateBeligerants ()

        member this.OnUpdate gameTime =
            do Option.iter (fun (battle: BattleState) -> battle.OnUpdate(gameTime)) battleState

        member this.OnRender () =
            desktop.Render()