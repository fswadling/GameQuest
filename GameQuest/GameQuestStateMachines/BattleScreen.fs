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

type ProgressBarActor (notifyComplete) =
    let progresBar = HorizontalProgressBar(Width=100)
    let mutable startTime: TimeSpan option = None
    interface IActor with
        member this.OnUpdate gameTime =
            match startTime with
            | None ->
                startTime <- Some gameTime.TotalGameTime
            | Some startTime ->
                let progress = (gameTime.TotalGameTime - startTime) / TimeSpan.FromSeconds(10.0)
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
    | Defend
    | UseItem
    | Flee

type TeamMemberActor (onActionChosen) =
    let mutable gameTime = TimeSpan.Zero
    let panel = VerticalStackPanel()
    let attackButton = Button(Content = Label(Text = "Attack"))
    do attackButton.Click.Add(fun _ -> onActionChosen (Attack, gameTime))
    let defendButton = Button(Content = Label(Text = "Defend"))
    do defendButton.Click.Add(fun _ -> onActionChosen (Defend, gameTime))
    let useItemButton = Button(Content = Label(Text = "Use Item"))
    do useItemButton.Click.Add(fun _ -> onActionChosen (UseItem, gameTime))
    let fleeButton = Button(Content = Label(Text = "Flee"))
    do fleeButton.Click.Add(fun _ -> onActionChosen (Flee, gameTime))
    do panel.Widgets.Add(attackButton)
    do panel.Widgets.Add(defendButton)
    do panel.Widgets.Add(useItemButton)
    do panel.Widgets.Add(fleeButton)

    interface IActor with
        member this.OnUpdate gt =
            gameTime <- gt.TotalGameTime
            ()

        member this.GetPanel () = 
            panel

type TeamMemberOrchestrationEvent =
   | ProgressBarComplete of TimeSpan
   | ChosenAction of TeamMemberAction * TimeSpan

type WholeTeamOrchestrationEvent = StoryShared.TeamMember * TeamMemberOrchestrationEvent

type BattleOrchestration = Orchestration<WholeTeamOrchestrationEvent, obj, (StoryShared.TeamMember * IActor)>

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

let wholeTeamTeamMemberOrchestration progressBarFactory teamMemberActorFactory teamMember = 
    event (
        function 
            | Some (tm, e) when tm = teamMember -> Some (Some e) 
            | Some _ -> None
            | None -> Some None)
    |> compose (teamMemberOrchestration (progressBarFactory teamMember) (teamMemberActorFactory teamMember))
    |> map (CircuitBreaker.mapBreak (List.map (fun y -> teamMember, y)))

let rec wholeTeamOrchestration progressBarFactory teamMemberActorFactory =
    List.map (wholeTeamTeamMemberOrchestration progressBarFactory teamMemberActorFactory)
    >> List.fold combine empty

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

type BattleState (battle: BattleOrchestration) =
    let actors = 
        lazy (
            let { CoordinationResult.Result = actions } = battle None

            actions
            |> List.choose (function | Break actors -> Some actors | _ -> None)
            |> List.collect id
            |> Map
            |> System.Collections.Generic.Dictionary
        )

    member this.TeamMemberActors with get() =
        actors.Value

    member this.DoEvent (event: WholeTeamOrchestrationEvent) =
        let { Next = next } = battle (Some event)
        Option.map (fun next -> BattleState (next)) next

    member this.OnUpdate gameTime =
        for (actor: IActor) in actors.Value.Values
            do actor.OnUpdate(gameTime)

type BattleScreen (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>, storyState: Story.State, gameState: GameState) =
    let mutable battleState: BattleState option = None
    let mutable teamPanels: System.Collections.Generic.Dictionary<_, _> option = None

    let getTeamPanel (battleState: BattleState) = 
        battleState.TeamMemberActors
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value, VerticalStackPanel())
        |> Seq.map (fun (tm, actor, panel) -> 
                do panel.Widgets.Add(actor.GetPanel())
                tm, panel)
        |> dict
        |> System.Collections.Generic.Dictionary

    let progressBarComplete teamMember gameTime =
        let newBattleState = 
            Option.bind (fun (x: BattleState) -> x.DoEvent(teamMember, TeamMemberOrchestrationEvent.ProgressBarComplete(gameTime))) battleState

        match newBattleState, teamPanels with
        | Some newBattleState, Some teamPanels ->
            let panel: VerticalStackPanel = teamPanels.[teamMember]
            do panel.Widgets.Clear()
            do panel.Widgets.Add(newBattleState.TeamMemberActors.[teamMember].GetPanel())
        | _ -> ()

        do battleState <- newBattleState

    let actionChosen teamMember (action, gameTime) =
        let newBattleState = 
            Option.bind (fun (x: BattleState) -> x.DoEvent(teamMember, ChosenAction(action, gameTime))) battleState

        match newBattleState, teamPanels with
        | Some newBattleState, Some teamPanels ->
            let panel: VerticalStackPanel = teamPanels.[teamMember]
            do panel.Widgets.Clear()
            do panel.Widgets.Add(newBattleState.TeamMemberActors.[teamMember].GetPanel())
        | _ -> ()

        do battleState <- newBattleState

    let progressBarActorFactory teamMember () = 
        new ProgressBarActor(progressBarComplete teamMember)
        :> IActor

    let teamMemberActorFactory teamMember () =
        new TeamMemberActor(actionChosen teamMember)
        :> IActor

    let battleOrchestration = 
        wholeTeamOrchestration
            progressBarActorFactory
            teamMemberActorFactory
            (Seq.toList storyState.CompanionsRecruited)

    let winBattle () =
        let newState = gameState.DoEvent (Story.StoryEvent.BattleWon)
        match newState with
        | Some newState -> updateScreenFn.Invoke(OpenGameScreen newState)
        | None -> ()

    let getRoot (teamPanels: System.Collections.Generic.Dictionary<_,_>) =
        let panel = Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = VerticalStackPanel()
        let winButtonLabel = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Win");
        let winButton = Button(Content = winButtonLabel)
        let teamStack = HorizontalStackPanel(VerticalAlignment = VerticalAlignment.Center)

        for kvp in teamPanels do
            let memberPanel = VerticalStackPanel()
            do memberPanel.Widgets.Add(Label(Text = kvp.Key.ToString()))
            do memberPanel.Widgets.Add(kvp.Value)
            do teamStack.Widgets.Add(memberPanel)

        winButton.TouchDown.Add(fun _ -> winBattle ())
        stack.Widgets.Add(winButton);
        stack.Widgets.Add(teamStack);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Dispose(): unit = 
            ()

        member this.Initialise () =
            let bs = BattleState(battleOrchestration)
            let panels = getTeamPanel bs
            do battleState <- Some bs
            do teamPanels <- Some (panels)
            desktop.Root <- getRoot panels

        member this.OnUpdate gameTime =
            do Option.iter (fun (battle: BattleState) -> battle.OnUpdate(gameTime)) battleState

        member this.OnRender () =
            desktop.Render()