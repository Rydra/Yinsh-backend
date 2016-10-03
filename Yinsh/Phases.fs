module Phases

open Yinshbackend.Domain
open BoardHelper
open System
open BusinessRules
open System
open Yinsh.Utils

let rec askForPosition() =
    let input = Console.ReadLine().Split(' ')
    match input with
    | [|letter; number|] ->
        let num = int(number)
        let position = { Letter = letter.ToUpper(); Number = num }
        position
    | _ ->
        printf "Invalid position provided, please write the two coordinates of the intersection you want to place the piece, separated by space: "
        askForPosition()

let playRemoveRing gameState =
    let ringPositions = 
        gameState.Board.Intersections 
        |> Seq.filter(fun kvp ->
            let value = kvp.Value
            match value.Status with
            | Filled(p) when p.Color = gameState.Active.Color && p.Type = Ring -> true
            | _ -> false)
        |> Seq.map(fun kvp -> kvp.Key)
        |> Seq.toList

    printfn "In which ring do you want to play? Positions: %A" (ringPositions |> List.map(fun p -> p.ToString()))

    let pos = askForPosition()

    let intersection = findIntersectionInBoard gameState.Board pos
    match intersection with
    | Some ({ Status = Filled(_) } as x) when ringPositions |> List.contains x.Position ->
        emptyIntersection gameState.Board x.Position
        let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)

        let updatedGameStatus =
            let winner = gameState.Players |> Array.tryFind(fun p -> p.CompletedRows >= 3)
            match winner with
            | Some p -> Finished p
            | None -> InProgress

        let updatedGameState = { gameState with Active = newActivePlayer; CurrentPhase = PlaceToken; GameStatus = updatedGameStatus }
        updatedGameState
    | _ ->
        printfn "Please provide the position of one of your rings"
        gameState

let playRemoveRow gameState rowsToRemove =
    let nRows = (rowsToRemove |> List.length)
    if nRows = 1 then
        rowsToRemove.[0] |> List.iter (fun p -> emptyIntersection gameState.Board p)
    else
        // TODO: Allow to choose row
        printfn "You formed several five in a row!"
        let optionDict = dict (List.zip [0 .. nRows] rowsToRemove)
        optionDict |> Seq.iter(fun kvp -> printfn "%i- %A" kvp.Key kvp.Value)
        let value = 0
        optionDict.[int value] |> List.iter (fun p -> emptyIntersection gameState.Board p)

    let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)

    let updatedGameStatus =
        let winner = gameState.Players |> Array.tryFind(fun p -> p.CompletedRows >= 3)
        match winner with
        | Some p -> Finished p
        | None -> InProgress

    let updatedGameState = { gameState with Active = newActivePlayer; CurrentPhase = RemoveRing; GameStatus = updatedGameStatus }
    updatedGameState

let playMoveRing gameState (intersection:Intersection) =

    let validRingMoves = getValidMoves gameState.Board intersection.Position

    printfn "Please choose any of the following positions to place the Ring: %A" (validRingMoves |> List.map(fun p -> p.ToString())) 

    let mutable placed = false
    let mutable newRingPosition = askForPosition()

    while not placed do
        if validRingMoves |> List.contains(newRingPosition) then
            let piece = { Color = gameState.Active.Color; Type = Ring }
            putPieceOnIntersection gameState.Board intersection.Position piece
            printfn "Token placed at %s" (intersection.Position.ToString())
            placed <- true
        else
            printfn "Please choose one coordinate from the valid move list"
            newRingPosition <- askForPosition()
    
    // Flip any intermediate tokens
    //getPath gameState.Board intersection.Position newRingPosition
    //|> List.iter(fun p -> flipToken gameState.Board p)

    // Detect any five in a row. TODO: We should iterate over this!
    let fiveInARow = findFiveInARow gameState.Board
    let nRows = (fiveInARow |> List.length)

    if nRows > 0 then
        let updatedGameState = { gameState with CurrentPhase = RemoveRows fiveInARow }
        updatedGameState
    else
        // No rows. Next player turn
        let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)
        let updatedGameState = { gameState with Active = newActivePlayer; CurrentPhase = PlaceToken; }
        updatedGameState

let playPlaceToken gameState =

    let ringPositions = 
        gameState.Board.Intersections 
        |> Seq.filter(fun kvp ->
            let value = kvp.Value
            match value.Status with
            | Filled(p) when p.Color = gameState.Active.Color && p.Type = Ring -> true
            | _ -> false)
        |> Seq.map(fun kvp -> kvp.Key)
        |> Seq.toList

    printfn "In which ring do you want to play? Positions: %A" (ringPositions |> List.map(fun p -> p.ToString()))

    let pos = askForPosition()

    let intersection = findIntersectionInBoard gameState.Board pos
    match intersection with
    | Some ({ Status = Filled(_) } as x) when ringPositions |> List.contains x.Position ->
        let piece = { Color = gameState.Active.Color; Type = Token }
        putPieceOnIntersection gameState.Board pos piece

        let validRingMoves = getValidMoves gameState.Board pos

        printfn "Please choose any of the following positions to place the Ring: %A" (validRingMoves |> List.map(fun p -> p.ToString())) 

        let piece = { Color = gameState.Active.Color; Type = Ring }

        let mutable placed = false
        let mutable newRingPosition = askForPosition()

        if validRingMoves |> List.contains(newRingPosition) then
            putPieceOnIntersection gameState.Board pos piece
            let updatedGameState = { gameState with CurrentPhase = MoveRing(x) }
            updatedGameState
        else 
            gameState
    | _ ->
        printfn "Please provide the position of one of your rings"
        gameState

let playRing gameState ringsPlaced =
    printf "Please, write the two coordinates of the intersection you want to place a ring, separated by space: "

    let pos = askForPosition()
    // Check if the intersection is empty

    let intersection = findIntersectionInBoard gameState.Board pos
    match intersection with
    | Some x when x.Status = Empty ->

        let piece = { Color = gameState.Active.Color; Type = Ring }
        putPieceOnIntersection gameState.Board pos piece

        printfn "%s ring placed at %s" (colorStr gameState.Active.Color) (pos.ToString())

        // Decide if we ended this phase
        let updatedPhase = 
            if ringsPlaced = 10 then 
                printfn "The 10 Rings have been placed. Let the game Begin!"
                PlaceToken
            else 
                PlaceRing(ringsPlaced + 1)


        let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)
        let updatedGameState = { gameState with Active = newActivePlayer; CurrentPhase = updatedPhase }

        updatedGameState
    | _ ->
        printfn "Please provide an empty intersection for ring placement"
        gameState