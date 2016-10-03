module Yss

open Yinshbackend.Domain
open BoardHelper
open System
open BusinessRules
open Phases

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

[<EntryPoint>]
let main argv = 
    let newGame = newGame()

    let rec play gameState =

        match gameState.GameStatus with
        | Finished(player) -> gameState
        | InProgress ->
            match gameState.CurrentPhase with
            | PlaceRing(ringsPlaced) ->
                let newGameState = playRing gameState ringsPlaced
                play newGameState
            | PlaceToken ->
                let newGameState = playPlaceToken gameState
                play newGameState
            | MoveRing(ringIntersection) ->
                let newGameState = playMoveRing gameState ringIntersection
                play newGameState
            | RemoveRows(rowsToRemove) ->
                let newGameState = playRemoveRow gameState rowsToRemove
                play newGameState
            | RemoveRing ->
                let newGameState = playRemoveRing gameState
                play newGameState

    play newGame |> ignore
    0 // return an integer exit code

