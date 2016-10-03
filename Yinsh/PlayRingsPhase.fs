
module PlayRingsPhase

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

let playRingPhaseTurn gameState ringsPlaced =
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
                Main 
            else 
                Start(ringsPlaced + 1)


        let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)
        let updatedGameState = { gameState with Active = newActivePlayer; CurrentPhase = updatedPhase }

        updatedGameState
    | _ ->
        printfn "Please provide an empty intersection for ring placement"
        gameState
