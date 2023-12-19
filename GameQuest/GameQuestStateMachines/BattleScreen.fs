module BattleScreen

open System
open Myra.Graphics2D.UI
open Microsoft.Xna.Framework
open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
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

type TeamMemberAction =
    | Attack
    | UsePotion

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

type TeamMemberOrchestrationEvent =
   | ProgressBarComplete of TimeSpan
   | ChosenAction of TeamMemberAction * TimeSpan

type WholeTeamOrchestrationEvent = StoryShared.TeamMember * TeamMemberOrchestrationEvent

let rec teamMemberOrchestration progressBarFactory teamMemberActorFactory = orchestration {
    let! progressCompleteTime = 
        raiseToOrchestrationWithActions
            [ Actor (progressBarFactory ()) ]
            (event (function | ProgressBarComplete gameTime -> Some gameTime | _ -> None))
            
    let! chosenAction, gameTime =
        raiseToOrchestrationWithActions
            [ Actor (teamMemberActorFactory ()) ]
            (event (function | ChosenAction (action, time) -> Some (action, time) | _ -> None))

    return! teamMemberOrchestration progressBarFactory teamMemberActorFactory
}

type EnemyEvent = 
    | ProgressBarComplete of TimeSpan
    | Attack of StoryShared.TeamMember

let rec enemyOrchestration progressBarFactory = orchestration {
    let! progressCompleteTime = 
        raiseToOrchestrationWithActions
            [ Actor (progressBarFactory ()) ]
            (event (function | ProgressBarComplete gameTime -> Some gameTime | _ -> None))

    do! raiseToOrchestrationWithActions
            [ Instant (Attack (StoryShared.TeamMember.You)) ]
            (event (function | Attack tm -> Some () | _ -> None))

    return! enemyOrchestration progressBarFactory
}

type BattleEvent =
    | TeamMemberEvent of WholeTeamOrchestrationEvent
    | EnemyEvent of EnemyEvent


type TeamMemberAndState = StoryShared.TeamMember * int
type EnemyState = int

type BattleOverState =
    | Victory
    | Defeat

type FullBattleState =
    { TeamMemberStates: TeamMemberAndState list
      EnemyState: EnemyState }

    static member Init teamMembers = 
        { TeamMemberStates = List.map (fun tm -> (tm, 100)) teamMembers
          EnemyState = 100 }

    member this.BattleOverState = 
        if (this.TeamMemberStates |> List.forall (fun (_, hp) -> hp <= 0)) then
            Some Defeat
        else if (this.EnemyState <= 0) then
            Some Victory
        else
            None

    member this.IsTeamMemberAlive tm =
        this.TeamMemberStates |> List.exists (fun (tm2, hp) -> tm = tm2 && hp > 0)

    member this.DeadTeamMembers =
        this.TeamMemberStates |> List.filter (fun (_, hp) -> hp <= 0) |> List.map fst

    member this.ReduceTeamMemberHealth tm damage =
        let teamMemberStates = 
            this.TeamMemberStates
            |> List.map (fun (tm', health) -> 
                if tm' = tm 
                then (tm', Math.Max(health - damage, 0)) 
                else (tm', health))
        { this with TeamMemberStates = teamMemberStates }

    member this.HealTeamMember tm =
        let teamMemberStates = 
            this.TeamMemberStates
            |> List.map (fun (tm', health) -> 
                if tm' = tm 
                then (tm', Math.Min(health + 50, 100)) 
                else (tm', health))
        { this with TeamMemberStates = teamMemberStates }

let wholeTeamTeamMemberOrchestration progressBarFactory teamMemberActorFactory teamMember = 
    event (
        function 
        | { Utilities.Event = Some (tm, e); Utilities.State = state: FullBattleState } 
            when tm = teamMember && state.IsTeamMemberAlive(tm) -> Some (Some e)
        | { Utilities.Event = None; Utilities.State = state: FullBattleState }
            when state.IsTeamMemberAlive(teamMember) -> Some None
        | _ -> None)
    |> compose (teamMemberOrchestration (progressBarFactory teamMember) (teamMemberActorFactory teamMember))
    |> map (CircuitBreaker.mapBreak (List.map (fun y -> teamMember, y)))

let rec wholeTeamOrchestration progressBarFactory teamMemberActorFactory =
    List.map (wholeTeamTeamMemberOrchestration progressBarFactory teamMemberActorFactory)
    >> List.fold combine empty

type FullBattleInteractive<'a> =
    | TeamMemberInteraction of StoryShared.TeamMember * BattleInteraction<'a>
    | EnemyInteraction of BattleInteraction<'a>

let raisedEnemyOrchestration enemyProgressBarFactory =
    event (function | None -> Some None | Some (EnemyEvent ee) -> Some (Some ee) | _ -> None)
    |> compose (
        (enemyOrchestration enemyProgressBarFactory) 
        |> mapBreak ((BattleInteraction.mapInstant EnemyEvent) >> EnemyInteraction))

let raisedWholeTeamOrchestration tmProgressBarFactory teamMemberActorFactory teamMembers =
    wholeTeamOrchestration tmProgressBarFactory teamMemberActorFactory teamMembers
    |> mapBreak (fun (tm, interaction) -> TeamMemberInteraction (tm, BattleInteraction.mapInstant TeamMemberEvent interaction))

let fullBattleOrc tmProgressBarFactory enemyProgressBarFactory teamMemberActorFactory teamMemberDeadActorFactory teamMembers = 
    event 
        (function 
            | { Utilities.Event = Some (TeamMemberEvent e); Utilities.State = s }
                -> Some { Utilities.Event = Some e; Utilities.State = s }
            | { Utilities.Event = None; Utilities.State = s } 
                -> Some { Utilities.Event = None; Utilities.State = s }
            | _ -> None)
        |> compose (raisedWholeTeamOrchestration tmProgressBarFactory teamMemberActorFactory teamMembers)
    |> combine
        (event Some
            |> map (fun { Utilities.Event = e } -> e)
            |> compose (raisedEnemyOrchestration enemyProgressBarFactory))
    |> combine
        (event
            (function 
                | { Utilities.Event = None; Utilities.State = s } 
                    -> Some s
                | _ -> None)
            |> map (fun (state: FullBattleState) -> 
                state.DeadTeamMembers 
                |> List.map (fun tm -> tm, (Actor (teamMemberDeadActorFactory ()))))
            |> map (List.map TeamMemberInteraction)
            |> map Break)
        

let private stateAccumulator updateState (eventAndState: Utilities.EventAndState<_,_>) = function
    | Some e -> { Utilities.State = updateState eventAndState.State e; Utilities.Event = Some e }
    | None -> { Utilities.State = eventAndState.State; Utilities.Event = None }

let updateState fullstate = function
    | TeamMemberEvent (tm, ChosenAction (TeamMemberAction.Attack, _)) ->
        let enemyState = fullstate.EnemyState - 10
        { fullstate with EnemyState = enemyState }
    | EnemyEvent (Attack tm) ->
        fullstate.ReduceTeamMemberHealth tm 10
    | TeamMemberEvent (tm, ChosenAction (TeamMemberAction.UsePotion, _)) ->
        fullstate.HealTeamMember tm
    | _ -> 
        fullstate

let fullBattleOrchestration initialState tmProgressBarFactory enemyProgressBarFactory teamMemberActorFactory teamMemberDeadActorFactory = 
    let teamMembers = initialState.TeamMemberStates |> List.map fst
    event Some
    |> scan (stateAccumulator updateState) ({ Event = None; State = initialState })
    |> skip 1
    |> compose 
        (combine
            (event Some
                |> compose (fullBattleOrc tmProgressBarFactory enemyProgressBarFactory teamMemberActorFactory teamMemberDeadActorFactory teamMembers))
            (event (function | { Utilities.State = state; Utilities.Event = Some _ } -> Some state | _ -> None)
                |> map (CircuitBreaker.retn)))

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
    let mutable (enemy: Beligerant option) = None

    let updateHealth 
        (enemyBeligerant: Beligerant) 
        (team: System.Collections.Generic.Dictionary<StoryShared.TeamMember, Beligerant>)
        (battleState: BattleState) =

        let tmStates = battleState.BattleState.TeamMemberStates

        for (tm, health) in tmStates do
            let exists, beligerant = team.TryGetValue(tm)
            if exists then
                do beligerant.UpdateHealth(health)

        let enemyHealth = battleState.BattleState.EnemyState

        do enemyBeligerant.UpdateHealth(enemyHealth)

    let tmProgressBarComplete teamMember gameTime =
        let e = TeamMemberEvent (teamMember, TeamMemberOrchestrationEvent.ProgressBarComplete(gameTime))

        let newBattleState = 
            Option.bind (fun (x: BattleState) -> x.DoEvent(e)) battleState

        if newBattleState.IsNone then
            ()
        else

        let beligerant = team.[teamMember]
        let teamMemberActor = newBattleState.Value.TeamMemberActors.[teamMember]
        do beligerant.UpdateActorPanel(teamMemberActor)
        do battleState <- newBattleState
        do updateHealth enemy.Value team newBattleState.Value


    let enemyProgressBarComplete gameTime =
        let e = EnemyEvent (EnemyEvent.ProgressBarComplete(gameTime))
        let newBattleState = Option.bind (fun (x: BattleState) -> x.DoEvent(e)) battleState

        if newBattleState.IsNone then
            ()
        else

        let enemyActor = newBattleState.Value.EnemyActor.Value
        do enemy.Value.UpdateActorPanel(enemyActor)

        for kvp in newBattleState.Value.TeamMemberActors do
            let beligerant = team.[kvp.Key]
            do beligerant.UpdateActorPanel(kvp.Value)

        do battleState <- newBattleState
        do updateHealth enemy.Value team newBattleState.Value

    let tmActionChosen teamMember (action, gameTime) =
        let e = TeamMemberEvent (teamMember, TeamMemberOrchestrationEvent.ChosenAction(action, gameTime))
        let newBattleState = Option.bind (fun (x: BattleState) -> x.DoEvent(e)) battleState

        if newBattleState.IsNone then
            ()
        else

        let beligerant = team.[teamMember]
        let teamMemberActor = newBattleState.Value.TeamMemberActors.[teamMember]
        do beligerant.UpdateActorPanel(teamMemberActor)
        do battleState <- newBattleState
        do updateHealth enemy.Value team newBattleState.Value

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

        for enemy: Beligerant in enemy |> Option.toList do
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
            do enemy <- Beligerant.GetEnemyBeligerant bs
            do desktop.Root <- getRoot ()
            do updateHealth enemy.Value team bs

        member this.OnUpdate gameTime =
            do Option.iter (fun (battle: BattleState) -> battle.OnUpdate(gameTime)) battleState

        member this.OnRender () =
            desktop.Render()