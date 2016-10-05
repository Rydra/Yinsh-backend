module Actions

open Domain
open BoardHelper
open System
open SearchAlgorithms
open System
open Yinsh.Utils

// Removes a ring from the specified position, and returns an updated
// gameState alongside a boolean indicating whether the removal was successful or there was a problem
let playRemoveRingAction gameState pos =

    let intersection = findIntersectionInBoard gameState.Board pos
    match intersection with
    | Some ({ Status = Filled({ Type = Ring }) } as x) ->
        emptyIntersection gameState.Board x.Position
        let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)

        let updatedGameStatus =
            let winner = gameState.Players |> Array.tryFind(fun p -> p.CompletedRows >= 3)
            match winner with
            | Some p -> Finished p
            | None -> InProgress

        let updatedGameState = { gameState with Active = newActivePlayer; CurrentPhase = PlaceToken; GameStatus = updatedGameStatus }
        (true, updatedGameState)
    | _ ->
        (false, gameState)

// Removes a set of points from the specified position in the board
// Warning: No check about whether the row positions are consecutive is made
let playRemoveRowAction board rowToRemove =
    rowToRemove |> List.iter (fun p -> emptyIntersection board p)