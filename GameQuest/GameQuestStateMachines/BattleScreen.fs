module BattleScreen

open System
open Myra.Graphics2D.UI
open Screens
open GameState
open System.Reactive.Subjects
open System.Reactive.Linq
open Microsoft.Xna.Framework

type TeamMemberState =
    | WaitingFrom of TimeSpan
    | Ready
    | Dead

// Use a mutable dictionary to avoid unnecessary allocations
type ProgressState () =
    let progress = System.Collections.Generic.Dictionary<StoryShared.TeamMember, float>()
    let completedSubject = new Subject<StoryShared.TeamMember>()

    member this.GetProgress(teamMember: StoryShared.TeamMember) = 
        progress.[teamMember]

    member this.SetProgress(teamMember: StoryShared.TeamMember, value: float) = 
        if progress.ContainsKey(teamMember) && progress.[teamMember] < 1.0 && value >= 1.0 then
            progress.[teamMember] <- 1.0
            completedSubject.OnNext(teamMember)
        else
            progress.[teamMember] <- value
 
    member this.Dispose(): unit =
        completedSubject.Dispose();

    member this.Completed with get () = completedSubject.AsObservable() 

    interface IDisposable with
        member this.Dispose(): unit =
            this.Dispose()

type TeamState(teamMembers: Set<StoryShared.TeamMember>, startTime: TimeSpan) =
    let teamState =
        teamMembers
        |> Seq.map(fun teamMember -> teamMember, WaitingFrom startTime) 
        |> dict
        |> System.Collections.Generic.Dictionary

    let progressState = new ProgressState()

    let completionSubscription = 
        progressState.Completed
        |> Observable.subscribe(
            fun teamMember ->
                teamState.[teamMember] <- Ready
                )
        
    member this.OnUpdate(time: GameTime) =
        for kvp in teamState do
            match kvp.Value with
            | WaitingFrom startTime ->
                let progress = (time.TotalGameTime - startTime) / TimeSpan.FromSeconds(10.0)
                progressState.SetProgress(kvp.Key, progress)
            | Ready ->
                progressState.SetProgress(kvp.Key, 1.0)
            | Dead ->
                progressState.SetProgress(kvp.Key, 0.0)

    member this.Dispose(): unit =
        completionSubscription.Dispose()
        progressState.Dispose();

    member this.GetProgress(teamMember: StoryShared.TeamMember) =
       progressState.GetProgress(teamMember)

    interface IDisposable with
        member this.Dispose(): unit =
            this.Dispose()
        

type BattleScreen (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>, storyState: Story.State, gameState: GameState) =
    let mutable teamState: Option<TeamState> = None

    let winBattle () =
        let newState = gameState.DoEvent (Story.StoryEvent.BattleWon)
        match newState with
        | Some newState -> updateScreenFn.Invoke(OpenGameScreen newState)
        | None -> ()

    let buildTeamMemberPanel teamMember =
        let panel = HorizontalStackPanel()
        let label = Label(Text = teamMember.ToString())
        let progressBar = HorizontalProgressBar(Width=100)
        panel.Widgets.Add(label)
        panel.Widgets.Add(progressBar)
        (panel, progressBar)

    let teamPanels =
        storyState.CompanionsRecruited
            |> Seq.map (fun teamMember -> teamMember, buildTeamMemberPanel teamMember)
            |> Map.ofSeq

    let updateTeamLabels () =
       for kvp in teamPanels do
            let _, progressBar = kvp.Value
            let progress = 
                teamState 
                |> Option.map (fun battleState -> (float32)(battleState.GetProgress(kvp.Key)))
                |> Option.defaultValue 0.0f
            progressBar.Value <- progress * 100.0f

    let teamPanel =
        teamPanels
            |> Seq.fold (fun (stack: StackPanel) memberPanel -> 
                let panel, _ = memberPanel.Value
                stack.Widgets.Add(panel)
                stack)
                (HorizontalStackPanel())

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
            if (teamState.IsSome) then
                teamState.Value.Dispose()

        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            if (teamState.IsNone) then
                teamState <- Some (new TeamState(storyState.CompanionsRecruited, gameTime.TotalGameTime))
            else
                do teamState.Value.OnUpdate gameTime
                do updateTeamLabels ()

        member this.OnRender () =
            desktop.Render()