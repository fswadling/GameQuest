open System

open OrchestrationCE.Coordination
open Story
open Utilities
open System.Text.RegularExpressions
open FSharp.Json
open System.IO

type TurnResult =
    | TurnEvent of Event
    | Save of string
    | Load of string

let loadGame eventsSoFar gameEvents =
    let rec loadGame' eventsSoFar gameEvents =
        match gameEvents with
        | [] -> eventsSoFar
        | event::remainingEvents -> 
            let { Next = next } = eventsSoFar (Some event)
            match next with
            | Some next -> loadGame' next remainingEvents
            | _ -> eventsSoFar
    loadGame' eventsSoFar (List.rev gameEvents)

let rec gameLoop eventsSoFar fullGame game doTurn =
    let { Result = actions } = game None
    
    let turnResult = doTurn actions

    match turnResult with
    | Save filename -> 
        File.WriteAllText(filename, Json.serialize(eventsSoFar))
        Console.WriteLine($"Game saved as {filename}")
        gameLoop eventsSoFar fullGame game doTurn
    | Load filename ->
        let filename = File.ReadAllText(filename)
        let events = Json.deserialize<Event list>(filename)
        let loadedGame = loadGame fullGame events
        gameLoop events fullGame loadedGame doTurn
    | TurnEvent event ->
        let { Result = _; Next = next } = game (Some event)
        match next with
        | Some next ->
            gameLoop (event::eventsSoFar) fullGame next doTurn
        | _ ->
            Console.WriteLine "Game over"

let rec doTurn possibleActions =
    let expositions, choices = 
        possibleActions
        |> List.partition (function | Exposition _ -> true | _ -> false)

    if not expositions.IsEmpty then
        match expositions with
        | Exposition (msg, event)::t ->
            Console.WriteLine (msg.ToString())
            match event with
            | Some e -> TurnEvent e
            | _ -> doTurn (t @ choices)
        | _ -> doTurn choices
    else

    let options = List.choose (function | Interactive (msg, event) -> Some (msg, event) | _ -> None) possibleActions

    Console.WriteLine "Possible actions:"

    do List.iteri (fun i (msg, event) -> Console.WriteLine $"{i}: {msg}") options

    Console.WriteLine "Choose an action"

    let line = Console.ReadLine()

    let optionIndex = 
        match (System.Int32.TryParse line) with
        | true, i when i < options.Length -> Some i 
        | _ -> None

    if optionIndex.IsSome then
        let _, event = options.[optionIndex.Value]
        TurnEvent event 
    else
        
    let saveRegexMatch = Regex(@"^save: (.+)$").Match(line)

    if saveRegexMatch.Success then
        let filename = saveRegexMatch.Groups.[1].Value
        Save filename
    else
            
    let loadRegexMatch = Regex(@"^load: (.+)$").Match(line)

    if loadRegexMatch.Success then
        // Extract the filename from the match
        let filename = loadRegexMatch.Groups.[1].Value
        Load filename
    else  
        
    Console.WriteLine "Invalid action"
    doTurn (List.map Interactive options)

Console.WriteLine("Echoes of Elaria: The Crystals of Destiny")

gameLoop [] story story doTurn

