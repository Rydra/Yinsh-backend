/// LRN: Each file must contain either a namespace or a module name. Inside modules
/// You can define variables and functions. Inside a Namespace you cannot. Personal style here.
/// If you come from imperative programming background or on a big project, You'll usually
/// use namespaces to better separate different parts of your code
module Phases

open Domain
open System
open System
open Yinsh.Utils
open Yinsh
open System.Diagnostics

let private MAX_RINGS_TO_PLACE = 10


let getPlayerBy color game =
    game.Players |> Array.find (fun p -> p.Color = color)

let private nextPlayerTurn game =
    let newActivePlayer = game |> getPlayerBy (invertColor game.Active.Color)
    { game with Active = newActivePlayer; }

let private detectFiveInARow board =
    let foundFiveInARow = Board.findFiveInARow board
    if (foundFiveInARow |> List.length) > 0 then (true, foundFiveInARow)
    else (false, [])

let private checkWinCondition game =
    let updatedGameStatus =
        let winner = game.Players |> Array.tryFind(fun p -> p.CompletedRows >= 3)
        match winner with
        | Some p -> Finished p
        | None -> InProgress

    { game with GameStatus = updatedGameStatus }

let private removeRow row board =
    row |> List.iter (fun p -> board |> Board.emptyIntersection p)

let private changeToRemoveRingPhase player game =
    { game with CurrentAction = RemoveRing ({ PlayerToRemoveRing = player }) }

let private changeToChooseRemoveRowPhase foundFiveInARow game =
    { game with CurrentAction = ChooseRowToRemove { RowsToRemove = foundFiveInARow } }

let private changeToRemoveRowPhase foundFiveInARow game =
    { game with CurrentAction = RemoveRow { RowToRemove = foundFiveInARow } }

let private changeToMoveRingPhase ringIntersection game =
    { game with CurrentAction = MoveRing({ RingToMove = ringIntersection }) }

let private changeToPlaceTokenPhase game =
    { game with CurrentAction = PlaceToken(PlaceTokenAction()); }

let private changeToAction action game =
    { game with CurrentAction = action; }

let private getRowColor row board =
    match Board.findIntersection board (row |> List.head) with
    | Some { Status = Filled(_ as piece) } -> piece.Color
    | _ -> failwith "Wrong!"
        
let private playRemoveRow game rowToRemove =
    let rowcolor = game.Board |> getRowColor rowToRemove
    game.Board |> removeRow rowToRemove
    game |> changeToRemoveRingPhase (game |> getPlayerBy rowcolor)

let private addCompletedRowToPlayer player game =
    let plIdx = game.Players |> Array.findIndex(fun p -> p = player)
    game.Players.[plIdx] <- { game.Players.[plIdx] with CompletedRows = game.Players.[plIdx].CompletedRows + 1 }

/// LRN: As you may see, even if F# is strongly and statically typed, you often do not need to declare
/// the types of your arguments and variables. This is because of the powerful type inference of F#.
/// Does that bother you? Novice developers usually require to see which type each variable/argument has 
/// in order to understand the code. Experienced developers only need to understand the CONCEPT behind
/// each variable/argument, giving special value to meaningful names. This results in code less polluted
/// and faster to read and understand.
let private removeRingFrom position (player: Player) game =

    let color = player.Color
    match Board.findIntersection game.Board position with
    | Some ({ Status = Filled({ Type = Ring; Color = color }) } as intersection) ->
        game.Board |> Board.emptyIntersection intersection.Position
        game |> addCompletedRowToPlayer player
        let game = game |> checkWinCondition

        let found, fiveInARow = game.Board |> detectFiveInARow

        if found then 
            if (fiveInARow |> List.length) = 1 then game |> changeToRemoveRowPhase (fiveInARow |> List.head)
            else game |> changeToChooseRemoveRowPhase fiveInARow
        else game |> nextPlayerTurn |> changeToPlaceTokenPhase
    | _ ->
        printfn "Please provide the position of one of your rings"
        game

let flipIntermediateTokens startPos endPos direction board =
    let rec constructPath currentIntersection path =
        if currentIntersection.Position = endPos then 
            path
        else
            match Board.getNextIntersectionInDir board currentIntersection.Position direction 1 with
            | Some intersection -> constructPath intersection (intersection.Position :: path)
            | None -> failwith "This should not be possible..."

    let nextMove = (Board.getNextIntersectionInDir board startPos direction 1).Value
    let path = constructPath nextMove []
    path |> List.iter(fun p -> Board.flipToken board p)

let private moveRingTo position (ringIntersection:Intersection) game =

    let validRingMoves =  game.Board |> Board.getValidMovesFrom ringIntersection.Position
    let _, validMoves = validRingMoves |> List.unzip

    if validMoves |> List.contains(position) |> not then
        game
    else
        game.Board |> Board.emptyIntersection ringIntersection.Position
        let piece = { Color = game.Active.Color; Type = Ring }
        game.Board |> Board.placePieceAt position piece

        // Determine direction to reach position
        let direction, _ = validRingMoves |> List.find(fun (_, pos) -> pos = position)
        game.Board |> flipIntermediateTokens ringIntersection.Position position direction

        let found, fiveInARow = game.Board |> detectFiveInARow

        if found then 
            if (fiveInARow |> List.length) = 1 then game |> changeToRemoveRowPhase (fiveInARow |> List.head)
            else game |> changeToChooseRemoveRowPhase fiveInARow
        else game |> nextPlayerTurn |> changeToPlaceTokenPhase

let private isValidPositionForTokenPlacement intersection game =
    let ringPositions = Board.findRingsInBoard game.Board game.Active
    match intersection with
     | { Status = Filled(_) } when ringPositions |> List.contains intersection.Position -> true
     | _ -> false

let private placeTokenAt position game =

    match Board.findIntersection game.Board position with
    | Some intersection when isValidPositionForTokenPlacement intersection game -> 
        let piece = { Color = game.Active.Color; Type = Token }
        game.Board |> Board.placePieceAt position piece
        game |> changeToMoveRingPhase intersection

    | _ ->
        printfn "Please provide the position of one of your rings"
        game

let private placeRingAt position game ringsCurrentlyPlaced =

    match Board.findIntersection game.Board position with
    | Some { Status = Empty } ->

        let piece = { Color = game.Active.Color; Type = Ring }
        game.Board |> Board.placePieceAt position piece

        Debug.WriteLine (sprintf "%s ring placed at %s" (colorStr game.Active.Color) (position.ToString()))

        let nextAction = 
            if ringsCurrentlyPlaced + 1 = MAX_RINGS_TO_PLACE then 
                Debug.WriteLine (sprintf "The 10 Rings have been placed. Let the game Begin!")
                PlaceToken(PlaceTokenAction())
            else 
                PlaceRing({RingsPlaced = ringsCurrentlyPlaced + 1 })

        game |> nextPlayerTurn |> changeToAction nextAction
    | _ ->
        Debug.WriteLine (sprintf "Please provide an empty intersection for ring placement")
        game

type ActionExecuter = Game -> Action -> Game

type PlaceRingAction with
    member this.DoAction game pos = placeRingAt pos game this.RingsPlaced

type PlaceTokenAction with
    member this.DoAction = placeTokenAt

type MoveRingAction with
    member this.DoAction = moveRingTo

type RemoveRowToRemoveAction with
    member this.DoAction = playRemoveRow

type RemoveRingAction with
    member this.DoAction = removeRingFrom

type ChooseRowToRemoveAction with
    member this.DoAction = changeToRemoveRowPhase 