/// LRN: Each file must contain either a namespace or a module name. Inside modules
/// You can define variables and functions. Inside a Namespace you cannot. Personal style here.
/// If you come from imperative programming background or on a big project, You'll usually
/// use namespaces to better separate different parts of your code
module Phases

open Domain
open System
open System
open Yinsh.Utils
open Actions
open Yinsh
open System.Diagnostics

/// LRN: As you may see, even if F# is strongly and statically typed, you often do not need to declare
/// the types of your arguments and variables. This is because of the powerful type inference of F#.
/// Does that bother you? Novice developers usually require to see which type each variable/argument has 
/// in order to understand the code. Experienced developers only need to understand the CONCEPT behind
/// each variable/argument, giving special value to meaningful names. This reasults in code less polluted
/// and faster to read and understand.
let playRemoveRing gameState pos =

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
        Debug.WriteLine "You formed several five in a row!"
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

let playMoveRing gameState (intersection:Intersection) newRingPosition =

    let validRingMoves = Board.getValidMoves gameState.Board intersection.Position
    let _, validMoves = validRingMoves |> List.unzip

    if validMoves |> List.contains(newRingPosition) |> not then
        gameState
    else
        // Flip any intermediate tokens
        let dir, pos = validRingMoves |> List.find(fun (_, pos) -> pos = newRingPosition)
        let rec constructPath currPos path =
            if currPos = pos then 
                path
            else
                let isect = Board.getNextIntersectionInDir gameState.Board intersection.Position dir 1
                match isect with
                | Some i -> constructPath i.Position (i.Position :: path)
                | None -> failwith "This should not be possible..."

        let nextMove = (Board.getNextIntersectionInDir gameState.Board intersection.Position dir 1).Value.Position
        let path = constructPath nextMove []
        path |> List.iter(fun p -> Board.flipToken gameState.Board p)

        // Detect any five in a row. TODO: We should iterate over this!
        let fiveInARow = Board.findFiveInARow gameState.Board
        let nRows = (fiveInARow |> List.length)

        if nRows > 0 then
            let updatedGameState = { gameState with CurrentPhase = RemoveRows fiveInARow }
            updatedGameState
        else
            // No rows. Next player turn
            let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)
            let updatedGameState = { gameState with Active = newActivePlayer; CurrentPhase = PlaceToken; }
            updatedGameState

let playPlaceToken gameState pos =

    let ringPositions = Board.findRingsInBoard gameState.Board gameState.Active
    let intersection = Board.findIntersectionInBoard gameState.Board pos
    match intersection with
    | Some ({ Status = Filled(_) } as x) when ringPositions |> List.contains x.Position ->
        let piece = { Color = gameState.Active.Color; Type = Token }
        Board.putPieceOnIntersection gameState.Board pos piece
        let updatedGameState = { gameState with CurrentPhase = MoveRing(x) }
        updatedGameState
    | _ ->
        printfn "Please provide the position of one of your rings"
        gameState

let playRing gameState ringsPlaced pos =

    // Validate the intersection if the intersection is empty
    let intersection = Board.findIntersectionInBoard gameState.Board pos
    match intersection with
    | Some { Status = Empty } as x ->

        let piece = { Color = gameState.Active.Color; Type = Ring }
        Board.putPieceOnIntersection gameState.Board pos piece

        Debug.WriteLine (sprintf "%s ring placed at %s" (colorStr gameState.Active.Color) (pos.ToString()))

        // Decide if we ended this phase
        let updatedPhase = 
            if ringsPlaced = 10 then 
                Debug.WriteLine (sprintf "The 10 Rings have been placed. Let the game Begin!")
                PlaceToken
            else 
                PlaceRing(ringsPlaced + 1)

        let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)
        let updatedGameState = { gameState with Active = newActivePlayer; CurrentPhase = updatedPhase }

        updatedGameState
    | _ ->
        Debug.WriteLine (sprintf "Please provide an empty intersection for ring placement")
        gameState