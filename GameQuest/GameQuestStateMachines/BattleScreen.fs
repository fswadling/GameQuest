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
    abstract member GetPanel: unit -> StackPanel

type ProgressBarActor (notifyComplete) =
    let progresBar = HorizontalProgressBar(Width=100)
    let mutable startTime: GameTime option = None
    interface IActor with
        member this.OnUpdate gameTime =
            match startTime with
            | None ->
                startTime <- Some gameTime
            | Some startTime ->
                let progress = (gameTime.TotalGameTime - startTime.TotalGameTime) / TimeSpan.FromSeconds(10.0)
                if (progress < 100.0) then
                    progresBar.Value <- (float32)progress * 100.0f
                else
                    notifyComplete gameTime

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
    let mutable gameTime = new GameTime()
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
            gameTime <- gt
            ()

        member this.GetPanel () = 
            panel

type TeamMemberOrchestrationEvent =
   | ProgressBarComplete of GameTime
   | ChosenAction of TeamMemberAction * GameTime

type WholeTeamOrchestrationEvent = StoryShared.TeamMember * TeamMemberOrchestrationEvent

type BattleOrchestration = Orchestration<WholeTeamOrchestrationEvent, obj, (StoryShared.TeamMember * IActor)>

let rec teamMemberOrchestration progressBarFactory teamMemberActorFactory = orchestration {
    let! progressCompleteTime = 
        raiseToOrchestrationWithActions
            [ progressBarFactory () ]
            (event (function | ProgressBarComplete gameTime -> Some gameTime | _ -> None))
            
    let! chosenAction, gameTime =
        raiseToOrchestrationWithActions
            [ teamMemberActorFactory () ]
            (event (function | ChosenAction (action, time) -> Some (action, time) | _ -> None))

    return! teamMemberOrchestration progressBarFactory teamMemberActorFactory
}

let wholeTeamTeamMemberOrchestration progressBarFactory teamMemberActorFactory teamMember = 
    event (function | Some (tm, e) when tm = teamMember -> Some (Some e) | _ -> None)
    |> compose (teamMemberOrchestration (progressBarFactory teamMember) (teamMemberActorFactory teamMember))
    |> map (CircuitBreaker.mapBreak (List.map (fun y -> teamMember, y)))

let rec wholeTeamOrchestration progressBarFactory teamMemberActorFactory =
    List.map (wholeTeamTeamMemberOrchestration progressBarFactory teamMemberActorFactory)
    >> List.fold combine empty

type BattleState (battle: BattleOrchestration) =
    let actors = 
        lazy (
            let { CoordinationResult.Result = actions } = battle None

            actions
            |> List.choose (function | Break actors -> Some actors | _ -> None)
            |> List.collect id
            |> List.map (fun (teamMember, actor) -> teamMember, actor.GetPanel())
            |> Map
            |> System.Collections.Generic.Dictionary
        )

    member this.TeamMemberActors with get() =
        actors.Value

    member this.DoEvent (event: WholeTeamOrchestrationEvent) =
        let { Next = next } = battle (Some event)
        Option.map (fun next -> BattleState (next)) next

    member this.OnUpdate gameTime =
        for (teamMember, actor: IActor) in actors.Value
            do actor.OnUpdate(gameTime)

type BattleScreen (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>, storyState: Story.State, gameState: GameState) =
    let mutable battleState: BattleState option = None

    let teamPanel, teamPanelDictionary = 
        let panel = HorizontalStackPanel()
        let teamMemberPanels = 
            battleState 
            |> Option.toList
            |> List.collect (fun battle -> 
                battle.TeamMemberActors
                |> List.map (fun (_, actor) -> actor.GetPanel()))

        for tmPanel in teamMemberPanels do
            panel.Widgets.Add(tmPanel)

        panel

    let progressBarComplete teamMember gameTime =
        do Option.iter (
            fun (battle: BattleState) -> 
                battleState <- battle.DoEvent(teamMember, ProgressBarComplete(gameTime))) 
                battleState

    let actionChosen teamMember (action, gameTime) =
        do Option.iter (
            fun (battle: BattleState) -> 
                battleState <- battle.DoEvent(teamMember, ChosenAction(action, gameTime)))
                battleState

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

    let root =
        let panel = Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = VerticalStackPanel()
        let winButtonLabel = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Win");
        let winButton = Button(Content = winButtonLabel)

        winButton.TouchDown.Add(fun _ -> winBattle ())
        stack.Widgets.Add(winButton);
        stack.Widgets.Add(teamPanel);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Dispose(): unit = 
            ()

        member this.Initialise () =
            do battleState <- Some (BattleState(battleOrchestration))
            desktop.Root <- root

        member this.OnUpdate gameTime =
            do Option.iter (fun (battle: BattleState) -> battle.OnUpdate(gameTime)) battleState

        member this.OnRender () =
            desktop.Render()