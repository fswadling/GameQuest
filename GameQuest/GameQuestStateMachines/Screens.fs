module Screens

open Utilities
open Myra.Graphics2D.UI
open Microsoft.Xna.Framework.Input
open Myra.Graphics2D.UI.File

type StartMenu (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>) =
    let root =
        let panel = new Panel()
        let stack = new VerticalStackPanel()

        let positionedText = new Label();
        positionedText.HorizontalAlignment <- HorizontalAlignment.Center;
        positionedText.Text <- "Echoes of Elaria: The Crystals of Destiny";

        let pressEnterText = new Label();
        pressEnterText.HorizontalAlignment <- HorizontalAlignment.Center;
        pressEnterText.Text <- "Press Enter";

        stack.Widgets.Add(positionedText);
        stack.Widgets.Add(pressEnterText);

        panel.VerticalAlignment <- VerticalAlignment.Center;
        panel.HorizontalAlignment <- HorizontalAlignment.Center;

        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            if (Keyboard.GetState().IsKeyDown(Keys.Enter))
            then updateFn.Invoke(TitleScreenDone)

        member this.OnRender () =
            desktop.Render()

type StartOrLoadMenu (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>) =
    let root =
        let panel = new Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = new VerticalStackPanel()
        let startNewGame = new Button(Content = new Label(Text = "Start new game"), HorizontalAlignment = HorizontalAlignment.Center)
        startNewGame.TouchDown.Add(fun _ -> updateFn.Invoke(StartLoadSelected {| DoLoad = false |}))
        let loadGame = new Button(Content = new Label(Text = "Load game"), HorizontalAlignment = HorizontalAlignment.Center)
        loadGame.TouchDown.Add(fun _ -> updateFn.Invoke(StartLoadSelected {| DoLoad = true |}))
        stack.Widgets.Add(startNewGame);
        stack.Widgets.Add(loadGame);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            ()

        member this.OnRender () =
            desktop.Render()

type LoadFileDialog (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>) =
    let dialog = FileDialog(FileDialogMode.OpenFile, Filter="*.json")

    let onLoadComplete () = 
        if dialog.Result
        then updateFn.Invoke(LoadFileResult (Some dialog.FilePath))
        else updateFn.Invoke(LoadFileResult None)
    
    interface IScreen with
        member this.Initialise () =
            dialog.Closed.Add(fun _ -> onLoadComplete ())
            dialog.ShowModal(desktop)

        member this.OnUpdate gameTime =
            ()

        member this.OnRender () =
            // I cant figure out why but it throws an exception when I cancel the dialog.
            try
                desktop.Render()
            with ex ->
                ()

type GameScreen (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>) =
    let mutable isEscButtonDown = true
    let root =
        let panel = new Panel()
        let stack = new VerticalStackPanel()

        let positionedText = new Label();
        positionedText.HorizontalAlignment <- HorizontalAlignment.Center;
        positionedText.Text <- "Game Screen";

        stack.Widgets.Add(positionedText);

        panel.VerticalAlignment <- VerticalAlignment.Center;
        panel.HorizontalAlignment <- HorizontalAlignment.Center;

        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            if (not isEscButtonDown && Keyboard.GetState().IsKeyDown(Keys.Escape))
            then
                isEscButtonDown <- true
                updateFn.Invoke(OpenMenuScreen)
            elif (isEscButtonDown && (Keyboard.GetState().IsKeyUp(Keys.Escape)))
            then
                isEscButtonDown <- false

        member this.OnRender () =
            desktop.Render()

type MenuScreen (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>) =
    let mutable isEscButtonDown = true
    let root =
        let panel = new Panel()
        let stack = new VerticalStackPanel()

        let positionedText = new Label();
        positionedText.HorizontalAlignment <- HorizontalAlignment.Center;
        positionedText.Text <- "Menu Screen";

        stack.Widgets.Add(positionedText);

        panel.VerticalAlignment <- VerticalAlignment.Center;
        panel.HorizontalAlignment <- HorizontalAlignment.Center;

        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            if (not isEscButtonDown && Keyboard.GetState().IsKeyDown(Keys.Escape))
            then
                isEscButtonDown <- true
                updateFn.Invoke(OpenGameScreen)
            elif (isEscButtonDown && (Keyboard.GetState().IsKeyUp(Keys.Escape)))
            then
                isEscButtonDown <- false

        member this.OnRender () =
            desktop.Render()