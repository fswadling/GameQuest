module BattleScreen

open System
open Myra.Graphics2D.UI
open Screens
open GameState
open Microsoft.Xna.Framework
open System.Collections.Generic

type TeamMemberAction =
    | StartWaiting
    | DoProgress of TimeSpan
    | ShowMenu

type TeamMemberState =
    struct
        val TeamMember: StoryShared.TeamMember
        val ProgressBar: HorizontalProgressBar
        val MessageQueue: System.Collections.Generic.Queue<TeamMemberAction>
        new (teamMember, progressBar, initialActions: TeamMemberAction seq) =
            { TeamMember = teamMember; ProgressBar = progressBar; MessageQueue = Queue<TeamMemberAction>(initialActions) }
    end 

    member this.BuildTeamMemberForm() =
        let panel = VerticalStackPanel()
        let label = Label(Text = teamMemberState.TeamMember.ToString())
        panel

    member this.OnUpdate(time: GameTime) =
        match this.MessageQueue.TryDequeue() with
        | true, TeamMemberAction.StartWaiting -> 
            do this.MessageQueue.Enqueue(TeamMemberAction.DoProgress time.TotalGameTime)
        | true, TeamMemberAction.DoProgress waitFrom when time.TotalGameTime - waitFrom > TimeSpan.FromSeconds(10.0) -> 
            ()
        | true, TeamMemberAction.DoProgress waitFrom ->
            let progress = (time.TotalGameTime - waitFrom) / TimeSpan.FromSeconds(10.0)
            this.ProgressBar.Value <- Math.Min((float32)progress * 100.0f, 100.0f)
            if (this.ProgressBar.Value < 100.0f) then
                do this.MessageQueue.Enqueue(TeamMemberAction.DoProgress waitFrom)
            else
                do this.MessageQueue.Enqueue(TeamMemberAction.ShowMenu)
        
        | false, _ -> ()


type BattleScreen (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>, storyState: Story.State, gameState: GameState) =
    let winBattle () =
        let newState = gameState.DoEvent (Story.StoryEvent.BattleWon)
        match newState with
        | Some newState -> updateScreenFn.Invoke(OpenGameScreen newState)
        | None -> ()

    let teamState =
        storyState.CompanionsRecruited
        |> Seq.map(fun teamMember -> 
            TeamMemberState(
                teamMember, 
                HorizontalProgressBar(Width=100),  
                [ TeamMemberAction.StartWaiting ]))
        |> Seq.toArray

    let onUpdate(time: GameTime) =
        for i in 0 .. teamState.Length - 1 do
            do teamState[i].OnUpdate(time)

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