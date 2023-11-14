module BattleScreen

open System
open Myra.Graphics2D.UI
open Screens
open GameState
open Microsoft.Xna.Framework

// Using a struct should in theory improve performance on the loop as all the team data will be colocated on the stack
type TeamMemberState =
    struct
        val TeamMember: StoryShared.TeamMember
        val ProgressBar: HorizontalProgressBar
        val mutable WaitFrom: TimeSpan
        val mutable AwaitingStart: bool
        
        new (teamMember, progressBar, waitFrom, awaitingStart) =
            { TeamMember = teamMember; ProgressBar = progressBar; WaitFrom = waitFrom; AwaitingStart = awaitingStart }
    end     

type BattleScreen (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>, storyState: Story.State, gameState: GameState) =
    let winBattle () =
        let newState = gameState.DoEvent (Story.StoryEvent.BattleWon)
        match newState with
        | Some newState -> updateScreenFn.Invoke(OpenGameScreen newState)
        | None -> ()

    let teamState =
        storyState.CompanionsRecruited
        |> Seq.map(fun teamMember -> TeamMemberState(teamMember, HorizontalProgressBar(Width=100), TimeSpan.Zero, true))
        |> Seq.toArray

    let onUpdate(time: GameTime) =
        for i in 0 .. teamState.Length - 1 do
            if (teamState.[i].AwaitingStart) then
                teamState.[i].WaitFrom <- time.TotalGameTime
                teamState.[i].AwaitingStart <- false
                teamState.[i].ProgressBar.Value <- 0.0f

            if (time.TotalGameTime - teamState.[i].WaitFrom > TimeSpan.FromSeconds(10.0)) then
                teamState.[i].ProgressBar.Value <- 1.0f * 100.0f
            else
                let progress = (time.TotalGameTime - teamState.[i].WaitFrom) / TimeSpan.FromSeconds(10.0)
                teamState.[i].ProgressBar.Value <- (float32)progress * 100.0f

    let teamPanel =
        let panel = HorizontalStackPanel()
        for teamMemberState in teamState do
            let label = Label(Text = teamMemberState.TeamMember.ToString())
            panel.Widgets.Add(label)
            panel.Widgets.Add(teamMemberState.ProgressBar)
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
            do onUpdate gameTime

        member this.OnRender () =
            desktop.Render()