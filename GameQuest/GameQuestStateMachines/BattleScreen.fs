module BattleScreen

open System
open Myra.Graphics2D.UI
open Screens
open GameState
open Microsoft.Xna.Framework

type IActor =
   abstract member this.OnUpdate gameTime : unit



type BattleMsg =
    | StartWaiting of HorizontalProgressBar
    | DoProgress of TimeSpan * float * HorizontalProgressBar
    | DisplayMenu of Button list

type TeamMemberMsg =
    { TeamMember: StoryShared.TeamMember; Message: BattleMsg  }

type BattleScreen (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>, storyState: Story.State, gameState: GameState) =
    let startMsgs = Seq.map (fun tm -> { TeamMember = tm; Message = StartWaiting (HorizontalProgressBar(Width=100)) }) storyState.CompanionsRecruited
    let queue = System.Collections.Generic.Queue<TeamMemberMsg>(startMsgs)

    let winBattle () =
        let newState = gameState.DoEvent (Story.StoryEvent.BattleWon)
        match newState with
        | Some newState -> updateScreenFn.Invoke(OpenGameScreen newState)
        | None -> ()

    let teamPanel, teamMap =
        storyState.CompanionsRecruited
        |> Seq.fold (fun ((fullPanel: StackPanel), (tmMap: Map<StoryShared.TeamMember, StackPanel>)) tm ->
            let fullCharPanel = VerticalStackPanel()
            fullPanel.Widgets.Add(fullCharPanel)
            let label = Label(Text = tm.ToString())
            do fullCharPanel.Widgets.Add(label)
            let charPanel = VerticalStackPanel()
            do fullCharPanel.Widgets.Add(charPanel)
            fullPanel, Map.add tm charPanel tmMap) (HorizontalStackPanel(), Map.empty)

    // TODO process all items in queue and return all new messages
    let rec processQueue consumed (time: GameTime) =
        let hasItem, item = queue.TryDequeue()
        if (not hasItem) then
            consumed, None
        else
        match item with
        | { TeamMember = tm; Message = StartWaiting progressBar } ->
            item::consumed, Some { TeamMember = tm; Message = DoProgress (time.TotalGameTime, 0.0, progressBar) }
        | { TeamMember = tm; Message = DoProgress (waitFrom, progress, progressBar) } ->
            if (progress < 100.0) then
                let newProgress = (time.TotalGameTime - waitFrom) / TimeSpan.FromSeconds(10.0)
                item::consumed, Some { TeamMember = tm; Message = DoProgress (waitFrom, newProgress, progressBar) }
            else
                processQueue consumed time

        | { TeamMember = teamMember; Message = DisplayMenu buttons } ->
            item::consumed, None

    let updateUI message =
        let { TeamMember = tm; Message = msg } = message
        let charPanel = Map.find tm teamMap
        match msg with
        | StartWaiting progressBar ->
            do charPanel.Widgets.Clear()
            do charPanel.Widgets.Add(progressBar)
        | DoProgress (_, progress, progressBar) ->
            do progressBar.Value <- (float32)progress * 100.0f
        | DisplayMenu buttons ->
            do charPanel.Widgets.Clear()
            do List.iter (fun button -> charPanel.Widgets.Add(button)) buttons

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
            desktop.Root <- root

        member this.OnUpdate gameTime =
            let consumed, newMsg = processQueue [] gameTime
            do List.iter updateUI consumed
            match newMsg with
            | Some newMsg -> 
                do queue.Enqueue newMsg
            | None -> ()

        member this.OnRender () =
            desktop.Render()