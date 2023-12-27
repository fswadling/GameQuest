module Screens

open Myra.Graphics2D.UI
open Microsoft.Xna.Framework.Input
open Myra.Graphics2D.UI.File
open System
open Microsoft.Xna.Framework
open OrchestrationCE.Coordination
open GameState

let wrapText (text: string) =
    text.Split(' ')
    |> Array.chunkBySize 10
    |> Array.map (fun x -> String.Join(" ", x)) 
    |> fun x -> String.Join("\n", x)

type ScreenJourneyEvent =
    | TitleScreenDone
    | StartLoadSelected of string option
    | OpenGameScreen of GameState
    | OpenMenuScreen of GameState
    | OpenBattleScreen of Story.State * GameState
    | OpenGameOverScreen of GameState

[<AllowNullLiteral>]
type IScreen =
    abstract member Initialise: unit -> unit
    abstract member OnUpdate: GameTime -> unit
    abstract member OnRender: unit -> unit

// Consumed within C# so using nulls rather than options
[<AllowNullLiteral>]
type ScreenManager (coordination) =
    let screen = 
        lazy (
            let { Result = screens } = coordination None 
            screens |> List.tryLast
        )

    member this.Screen with get() : IScreen = if screen.Value.IsSome then screen.Value.Value else null

    member this.DoStep (e: ScreenJourneyEvent) =
        let { Next = next } = coordination (Some e)
        next 
        |> Option.map ScreenManager
        |> Option.defaultValue null

type StartMenu (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>) =
    let root =
        let panel = Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = VerticalStackPanel()

        let positionedText = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Echoes of Elaria: The Crystals of Destiny")
        let pressEnterText = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Press Enter");

        stack.Widgets.Add(positionedText);
        stack.Widgets.Add(pressEnterText);

        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            if (Keyboard.GetState().IsKeyDown(Keys.Enter))
            then updateScreenFn.Invoke(TitleScreenDone)

        member this.OnRender () =
            desktop.Render()

type StartOrLoadMenu (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>) =
    let mutable selectedFilePath = None
    let onLoadComplete (dialog: FileDialog) = 
        if dialog.Result
        then selectedFilePath <- Some dialog.FilePath

    let root =
        let dialog = FileDialog(FileDialogMode.OpenFile, Filter="*.json")
        dialog.Closed.Add(fun _ -> onLoadComplete dialog)

        let panel = Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = VerticalStackPanel()
        let startNewGame = Button(Content = Label(Text = "Start new game"), HorizontalAlignment = HorizontalAlignment.Center)

        startNewGame.TouchDown.Add(fun _ -> updateScreenFn.Invoke(StartLoadSelected None))
        let loadGame = Button(Content = Label(Text = "Load game"), HorizontalAlignment = HorizontalAlignment.Center)
        loadGame.TouchDown.Add(fun _ -> dialog.ShowModal(desktop))

        stack.Widgets.Add(startNewGame);
        stack.Widgets.Add(loadGame);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            match selectedFilePath with
            | Some filePath -> updateScreenFn.Invoke(StartLoadSelected (Some filePath))
            | None -> ()

        member this.OnRender () =
            desktop.Render()

type GameScreen (desktop: Desktop, updateScreenFn: System.Action<ScreenJourneyEvent>, gameState: GameState.GameState) =
    let mutable gameState = gameState
    let mutable escKeyDown = Keyboard.GetState().IsKeyDown(Keys.Escape)

    let doEvent buildScreen event =
        match gameState.DoEvent event with
        | Some newState -> 
            match newState.Battle with
            | None ->
                gameState <- newState
                desktop.Root <- (buildScreen newState)
            | Some battleState ->
                updateScreenFn.Invoke(OpenBattleScreen (battleState, newState))
        | None -> ()

    let rec buildScreen (gameState: GameState) =
        let panel = Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = VerticalStackPanel()
        panel.Widgets.Add(stack);

        let eventExpositions = gameState.EventExpositions
        if eventExpositions.Length > 0 then
            let exposition, event = eventExpositions.Head
            let text = wrapText (exposition.ToString())
            let positionedText = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = text);
            let continueBtn = Button(Content = Label(Text = "Continue"), HorizontalAlignment = HorizontalAlignment.Center)
            continueBtn.TouchDown.Add(fun _ -> doEvent buildScreen event)
            stack.Widgets.Add(positionedText);
            stack.Widgets.Add(continueBtn);
            panel
        else

        for exposition in gameState.NonEventExpositions do
            let text = wrapText (exposition.ToString())
            let label = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = text)
            stack.Widgets.Add(label)

        for (interaction, event) in gameState.Interactives do
            let label = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = interaction.ToString())
            let button = Button(Content = label)
            button.TouchDown.Add(fun _ -> doEvent buildScreen event)
            stack.Widgets.Add(button)

        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Initialise () =
            desktop.Root <- buildScreen gameState
            
        member this.OnUpdate gameTime =
            let escKeyState = Keyboard.GetState().IsKeyDown(Keys.Escape)
            if escKeyState && not escKeyDown then 
                escKeyDown <- true
                updateScreenFn.Invoke(OpenMenuScreen gameState)
            else if not escKeyState && escKeyDown then 
                escKeyDown <- false

        member this.OnRender () =
            desktop.Render()

type MenuScreen (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>, gameState: GameState, story: Story.Story) =
    let mutable gameState = gameState
    let mutable escKeyDown = Keyboard.GetState().IsKeyDown(Keys.Escape)

    let saveGame () =
        let dialog = FileDialog(FileDialogMode.SaveFile, Filter="*.json")

        dialog.Closed.Add(fun _ -> 
            if dialog.Result
            then gameState.Save(dialog.FilePath))

        do dialog.ShowModal(desktop)

    let loadGame () =
        let dialog = FileDialog(FileDialogMode.OpenFile, Filter="*.json")

        dialog.Closed.Add(fun _ -> 
            if dialog.Result 
            then 
                let newGameState = GameState.Load(dialog.FilePath, story)
                if newGameState.IsSome
                then gameState <- newGameState.Value)

        do dialog.ShowModal(desktop)

    let root =
        let panel = Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = VerticalStackPanel()
        let saveButtonLabel = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Save");
        let saveButton = Button(Content = saveButtonLabel)
        saveButton.TouchDown.Add(fun _ -> saveGame ())
        let loadButtonLabel = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Load");
        let loadButton = Button(Content = loadButtonLabel)
        loadButton.TouchDown.Add(fun _ -> loadGame ())
        stack.Widgets.Add(saveButton);
        stack.Widgets.Add(loadButton);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            let escKeyState = Keyboard.GetState().IsKeyDown(Keys.Escape)
            if escKeyState && not escKeyDown then 
                escKeyDown <- true
                updateFn.Invoke(OpenGameScreen gameState)
            else if not escKeyState && escKeyDown then 
                escKeyDown <- false

        member this.OnRender () =
            desktop.Render()

type GameOverScreen (desktop: Desktop) =
    let root =
        let panel = Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = VerticalStackPanel()
        let textLabel = Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Game Over");
        stack.Widgets.Add(textLabel);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            ()

        member this.OnRender () =
            desktop.Render()

