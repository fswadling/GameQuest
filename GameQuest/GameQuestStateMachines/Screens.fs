module Screens

open Utilities
open Myra.Graphics2D.UI
open Microsoft.Xna.Framework.Input
open Myra.Graphics2D.UI.File
open System
open System.Reactive.Subjects
open System.Reactive.Linq

type StartMenu (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>) =
    let root =
        let panel = new Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = new VerticalStackPanel()

        let positionedText = new Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Echoes of Elaria: The Crystals of Destiny")
        let pressEnterText = new Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Press Enter");

        stack.Widgets.Add(positionedText);
        stack.Widgets.Add(pressEnterText);

        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Dispose(): unit = 
            ()

        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            if (Keyboard.GetState().IsKeyDown(Keys.Enter))
            then updateFn.Invoke(TitleScreenDone)

        member this.OnRender () =
            desktop.Render()

type StartOrLoadMenu (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>) =
    let mutable selectedFilePath = None
    let onLoadComplete (dialog: FileDialog) = 
        if dialog.Result
        then selectedFilePath <- Some dialog.FilePath

    let root =
        let dialog = FileDialog(FileDialogMode.OpenFile, Filter="*.json")
        dialog.Closed.Add(fun _ -> onLoadComplete dialog)

        let panel = new Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = new VerticalStackPanel()
        let startNewGame = new Button(Content = new Label(Text = "Start new game"), HorizontalAlignment = HorizontalAlignment.Center)

        startNewGame.TouchDown.Add(fun _ -> updateFn.Invoke(StartLoadSelected None))
        let loadGame = new Button(Content = new Label(Text = "Load game"), HorizontalAlignment = HorizontalAlignment.Center)
        loadGame.TouchDown.Add(fun _ -> dialog.ShowModal(desktop))

        stack.Widgets.Add(startNewGame);
        stack.Widgets.Add(loadGame);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Dispose(): unit = 
            ()

        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            match selectedFilePath with
            | Some filePath -> updateFn.Invoke(StartLoadSelected (Some filePath))
            | None -> ()

        member this.OnRender () =
            desktop.Render()

type GameScreen (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>) =
    let escKeySubject = new Subject<bool>()
    let subscription = 
        escKeySubject
            .Buffer(2, 1)
            .Select(List.ofSeq)
            .Subscribe(function | [false; true] -> updateFn.Invoke(OpenMenuScreen) | _ -> ())

    let root =
        let panel = Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = VerticalStackPanel()

        let positionedText = new Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Game Screen");
        stack.Widgets.Add(positionedText);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Dispose(): unit = 
            subscription.Dispose()
            escKeySubject.Dispose()

        member this.Initialise () =
            desktop.Root <- root
            
        member this.OnUpdate gameTime =
            escKeySubject.OnNext(Keyboard.GetState().IsKeyDown(Keys.Escape))

        member this.OnRender () =
            desktop.Render()

type MenuScreen (desktop: Desktop, updateFn: System.Action<ScreenJourneyEvent>) =
    let escKeySubject = new Subject<bool>()
    let subscription = 
        escKeySubject
            .Buffer(2, 1)
            .Select(List.ofSeq)
            .Subscribe(function | [false; true] -> updateFn.Invoke(OpenGameScreen) | _ -> ())
    let root =
        let panel = new Panel(VerticalAlignment = VerticalAlignment.Center, HorizontalAlignment = HorizontalAlignment.Center)
        let stack = new VerticalStackPanel()
        let positionedText = new Label(HorizontalAlignment = HorizontalAlignment.Center, Text = "Menu Screen");

        stack.Widgets.Add(positionedText);
        panel.Widgets.Add(stack);
        panel

    interface IScreen with
        member this.Dispose(): unit = 
            subscription.Dispose()
            escKeySubject.Dispose()

        member this.Initialise () =
            desktop.Root <- root

        member this.OnUpdate gameTime =
            escKeySubject.OnNext(Keyboard.GetState().IsKeyDown(Keys.Escape))

        member this.OnRender () =
            desktop.Render()