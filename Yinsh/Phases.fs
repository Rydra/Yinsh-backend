/// LRN: Each file must contain either a namespace or a module name. Inside modules
/// You can define variables and functions. Inside a Namespace you cannot. Personal style here.
/// If you come from imperative programming background or on a big project, You'll usually
/// use namespaces to better separate different parts of your code
module Phases

open Domain
open BoardHelper
open System
open SearchAlgorithms
open System
open Yinsh.Utils
open Actions

let rec askForPosition() =
    let input = Console.ReadLine().Split(' ')
    match input with
    | [|letter; number|] when Int32.TryParse number |> fst ->
        let num = int(number)
        let position = { Letter = letter.ToUpper(); Number = num }
        position
    | _ ->
        printf "Invalid position provided, please write the two coordinates of the intersection you want to place the piece, separated by space: "
        askForPosition()

/// LRN: As you may see, even if F# is strongly and statically typed, you often do not need to declare
/// the types of your arguments and variables. This is because of the powerful type inference of F#.
/// Does that bother you? Novice developers usually require to see which type each variable/argument has 
/// in order to understand the code. Experienced developers only need to understand the CONCEPT behind
/// each variable/argument, giving special value to meaningful names. This reasults in code less polluted
/// and faster to read and understand.
let playRemoveRing gameState =

    // Get a list of the positions of rings for
    // the active player
    // LRN: If you come from a background where you have strong functional query libraries
    // like LINQ for C# or underscoreJS in Javascript, F# has the List, Seq among other modules
    // that contain a lot of functions to manipulate collections of objects. In this example,
    // filter = where from LINQ, map = Select from LINQ, etc. I personally find the F# more powerful
    // thanks to the pipeline |> and no need of using extension methods.
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

    // See how we can easily return and split tuples to different variables
    let ok, newgameState = playRemoveRingAction gameState pos

    if ok then
        newgameState
    else
        printfn "Please provide the position of one of your rings"
        gameState

let playRemoveRow gameState rowsToRemove =
    let nRows = (rowsToRemove |> List.length)
    if nRows = 1 then
        playRemoveRowAction gameState.Board rowsToRemove.[0]
    else
        // TODO: Allow to choose row
        printfn "You formed several five in a row!"
        let optionDict = dict (List.zip [0 .. nRows] rowsToRemove)
        optionDict |> Seq.iter(fun kvp -> printfn "%i- %A" kvp.Key kvp.Value)
        let value = 0
        playRemoveRowAction gameState.Board optionDict.[int value]

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
    let _, validMoves = validRingMoves |> List.unzip

    printfn "Please choose any of the following positions to place the Ring: %A" (validRingMoves |> List.map(fun p -> p.ToString())) 

    let mutable placed = false
    let mutable newRingPosition = askForPosition()

    while not placed do
        if validMoves |> List.contains(newRingPosition) then
            let piece = { Color = gameState.Active.Color; Type = Ring }
            putPieceOnIntersection gameState.Board intersection.Position piece
            printfn "Token placed at %s" (intersection.Position.ToString())
            placed <- true
        else
            printfn "Please choose one coordinate from the valid move list"
            newRingPosition <- askForPosition()
    
    // Flip any intermediate tokens
    let dir, pos = validRingMoves |> List.find(fun (_, pos) -> pos = newRingPosition)
    let rec constructPath currPos path =
        if currPos = pos then 
            path
        else
            let isect = getNextIntersectionInDir gameState.Board intersection.Position dir 1
            match isect with
            | Some i -> constructPath i.Position (i.Position :: path)
            | None -> failwith "This should not be possible..."

    let nextMove = (getNextIntersectionInDir gameState.Board intersection.Position dir 1).Value.Position
    let path = constructPath nextMove []
    path |> List.iter(fun p -> flipToken gameState.Board p)

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

    printfn "In which ring do you want to play a Token? Positions: %A" (ringPositions |> List.map(fun p -> p.ToString()))

    let pos = askForPosition()

    let intersection = findIntersectionInBoard gameState.Board pos
    match intersection with
    | Some ({ Status = Filled(_) } as x) when ringPositions |> List.contains x.Position ->
        let piece = { Color = gameState.Active.Color; Type = Token }
        putPieceOnIntersection gameState.Board pos piece
        let updatedGameState = { gameState with CurrentPhase = MoveRing(x) }
        updatedGameState
    | _ ->
        printfn "Please provide the position of one of your rings"
        gameState

let playRing gameState ringsPlaced =
    
    let pos = askForPosition()
    // Check if the intersection is empty

    let intersection = findIntersectionInBoard gameState.Board pos
    match intersection with
    | Some { Status = Empty } as x ->

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