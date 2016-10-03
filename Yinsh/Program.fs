// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Yinshbackend.Domain
open BoardHelper
open System
open BusinessRules
open PlayRingsPhase
open PlaceTokensPhase

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
            | Start(ringsPlaced) ->
                let newGameState = playRingPhaseTurn gameState ringsPlaced
                play newGameState
            | Main ->
                let newGameState = playPlaceTokenTurn gameState
                play newGameState
            | _ ->
                printfn "Please provide an empty intersection for marker placement"
                play gameState

    play newGame |> ignore
    0 // return an integer exit code

