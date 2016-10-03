// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Yinshbackend.Domain
open BoardHelper
open System
open BusinessRules

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

let rec repeatUntil condition func =
    let value = func()
    if condition(value) then
        value
    else repeatUntil condition func
        

[<EntryPoint>]
let main argv = 

    let newGame = newGame()

    let rec play gameState =

        match gameState.GameStatus with
        | Finished(player) -> gameState
        | InProgress ->
            match gameState.CurrentPhase with
            | Start(ringsPlaced) ->
                printf "Please, write the two coordinates of the intersection you want to place a ring, separated by space: "
                let pos = askForPosition()
                // Check if the intersection is empty

                let intersection = findIntersectionInBoard gameState.Board pos
                match intersection with
                | None ->
                    printfn "Please provide an empty intersection for ring placement"
                    play gameState
                | Some x ->
                    match x.Status with
                    | Empty ->
                        let piece = { Color = gameState.Active.Color; Type = Ring }
                        putPieceOnIntersection gameState.Board pos piece

                        let updatedPhase =
                            if ringsPlaced = 10 then
                                Main
                            else 
                                Start(ringsPlaced + 1)
                            
                        let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)

                        let updatedGameState = { gameState with Active = newActivePlayer; CurrentPhase = updatedPhase }
                        play updatedGameState
                    | _ ->
                        printfn "Please provide an empty intersection for ring placement"
                        play gameState
            | Main ->
                let pos = askForPosition()

                let intersection = findIntersectionInBoard gameState.Board pos
                match intersection with
                | None ->
                    printfn "Please provide an empty intersection for token placement"
                    play gameState
                | Some x ->
                    match x.Status with
                    | Empty ->
                        let piece = { Color = gameState.Active.Color; Type = Token }
                        putPieceOnIntersection gameState.Board pos piece

                        let validRingMoves = getValidMoves gameState.Board pos

                        printfn "Please choose any of the following positions to place the Ring: %A" validRingMoves

                        let piece = { Color = gameState.Active.Color; Type = Ring }

                        let mutable placed = false
                        let mutable newRingPosition = askForPosition()

                        while not placed do
                            if validRingMoves |> List.contains(newRingPosition) then
                                putPieceOnIntersection gameState.Board pos piece
                                placed <- true
                            else
                                printfn "Please choose one coordinate from the valid move list"
                                newRingPosition <- askForPosition()

                        
                        // Flip any intermediate tokens
                        getPath gameState.Board pos newRingPosition
                        |> List.iter(fun p -> flipToken gameState.Board p)

                        // Detect any five in a row. TODO: We should iterate over this!
                        let fiveInARow = findFiveInARow gameState.Board

                        let nRows = (fiveInARow |> List.length)
                        if nRows > 0 then
                            if nRows = 1 then
                                fiveInARow.[0] |> List.iter (fun p -> emptyIntersection gameState.Board p)
                            else
                                printfn "You formed several five in a row!"
                                let optionDict = dict (List.zip [0 .. nRows] fiveInARow)
                                optionDict |> Seq.iter(fun kvp -> printfn "%i- %A" kvp.Key kvp.Value)
                                let value = 
                                    repeatUntil (fun v -> (Int32.TryParse(v) |> fst) && optionDict.ContainsKey(v |> int)) (fun _ -> 
                                        printf "Please choose one of the row numbers: "
                                        Console.ReadLine())

                                optionDict.[int value] |> List.iter (fun p -> emptyIntersection gameState.Board p)
                                

                            
                        let newActivePlayer = gameState.Players |> Array.find (fun p -> p.Color = invertColor gameState.Active.Color)

                        let updatedGameStatus =
                            let winner = gameState.Players |> Array.tryFind(fun p -> p.CompletedRows >= 3)
                            match winner with
                            | Some p -> Finished p
                            | None -> InProgress

                        let updatedGameState = { gameState with Active = newActivePlayer; GameStatus = updatedGameStatus }
                        play updatedGameState
                    | _ ->
                        printfn "Please provide an empty intersection for marker placement"
                        play gameState



    play newGame |> ignore
    0 // return an integer exit code

