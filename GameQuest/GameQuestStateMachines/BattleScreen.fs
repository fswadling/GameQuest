module BattleScreen

open System
open Myra.Graphics2D.UI
open Microsoft.Xna.Framework
open OrchestrationCE.Coordination
open OrchestrationCE.Orchestration
open BattleSystem
open BattleState
open Screens
open GameState

type IActor =
    abstract member OnUpdate: gameTime: GameTime -> unit
    abstract member GetPanel: unit -> VerticalStackPanel

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

type BattleState (battle: BattleOrchestration<IActor>, state, winBattle, loseBattle) =
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
                    | TeamMemberInteraction (_, actor) 
                    | EnemyInteraction (Actor actor) -> Some actor | _ -> None)
            |> List.toArray
        )

    member this.BattleState with get() =
        state

    member this.TeamMemberActors with get() =
        interactives.Value
        |> List.choose (function | TeamMemberInteraction (tm, actor) -> Some (tm, actor) | _ -> None)
        |> dict
        |> System.Collections.Generic.Dictionary

    member this.EnemyActor with get() =
        interactives.Value
        |> List.choose (function | EnemyInteraction (Actor actor) -> Some actor | _ -> None)
        |> List.tryLast

    member this.DoEvent (event: BattleEvent) =
        let { Result = result; Next = next } = battle (Some event) 
        let state = 
            result
            |> List.choose (function | Continue x -> Some x | _ -> None)
            |> List.tryLast

        match state, next with
        | None, _ ->
            failwith "Battle orchestration should always return state upon applying an event"
        | Some Victory, _ ->
            winBattle()
            None
        | Some Defeat, _ ->
            loseBattle()
            None
        | _, None ->
            failwith "Battle orchestration should be infinite sequence"
        | Some state, Some next ->
            Some (BattleState(next, state, winBattle, loseBattle))

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
            (BattleState.Init (storyState.CompanionsRecruited |> Set.toList))
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
            let state = BattleState.Init (storyState.CompanionsRecruited |> Set.toList)
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