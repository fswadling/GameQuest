module BattleScreen

open System
open Myra.Graphics2D.UI
open Screens
open GameState
open Microsoft.Xna.Framework

type BattleMsg =
    | StartWaiting
    | DoProgress of TimeSpan

type TeamMemberMsg =
    { TeamMember: StoryShared.TeamMember; Message: BattleMsg  }

type BattleScreen (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>, storyState: Story.State, gameState: GameState) =
    let startMsgs = Seq.map (fun tm -> { TeamMember = tm; Message = StartWaiting }) storyState.CompanionsRecruited
    let queue = System.Collections.Generic.Queue<TeamMemberMsg>(startMsgs)

    let winBattle () =
        let newState = gameState.DoEvent (Story.StoryEvent.BattleWon)
        match newState with
        | Some newState -> updateScreenFn.Invoke(OpenGameScreen newState)
        | None -> ()

    let teamState =
        storyState.CompanionsRecruited
        |> Seq.map(fun teamMember -> 
            teamMember,
            HorizontalProgressBar(Width=100))
        |> dict

    let rec processQueue (time: GameTime) =
        match queue.TryDequeue() with
        | true, { TeamMember = tm; Message = StartWaiting } -> 
            Some { TeamMember = tm; Message = DoProgress time.TotalGameTime }
        | true, { TeamMember = tm; Message = DoProgress waitFrom } ->
            let progress = (time.TotalGameTime - waitFrom) / TimeSpan.FromSeconds(10.0)
            let progressBar = teamState.[tm] 
            if (progressBar.Value < 100.0f) then
                progressBar.Value <- (float32)(progress * 100.0)
                Some { TeamMember = tm; Message = DoProgress waitFrom }
            else
                processQueue time
        | false, _ -> None

    let teamPanel =
        let panel = HorizontalStackPanel()
        for teamMemberState in teamState do
            let label = Label(Text = teamMemberState.Key.ToString())
            panel.Widgets.Add(label)
            panel.Widgets.Add(teamMemberState.Value)
        panel

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
            let msg = processQueue gameTime
            match msg with
            | Some msg -> queue.Enqueue msg
            | None -> ()

        member this.OnRender () =
            desktop.Render()