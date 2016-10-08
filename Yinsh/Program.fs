/// LRN: "open" imports a module or namespace to have the functions
/// or classes inside the module available
open Domain
open System
open Phases
open Yinsh.Rendering
open Yinsh
open GameHelper

let rec askForPosition() =
    let input = Console.ReadLine().Split(' ')
    match input with
    | [|letter; number|] ->
        let num = int(number)
        let position = Position (letter.ToUpper()) num
        position
    | _ ->
        printf "Invalid position provided, please write the two coordinates of the intersection you want to place the piece, separated by space: "
        askForPosition()

/// LRN: Every time you find in the code a comment with the letters LRN (Learn) it is a comment 
/// with explanations about certain parts of code particular to functional programming and F#

/// LRN: EntryPoint annotation indicates the main function of the program. The main function 
/// has one argument, the argv
[<EntryPoint>]
let main argv = 
    let newGame = newGame()
    let size = { Width = 80; Height = 50 }

    /// LRN: You can implement nested functions inside functions.
    /// These functions are private to the parent function and not exposed.
    /// In this case, it is a recursive helper function. In F# all recursive
    /// functions require the "rec" keyword
    let rec play gameState =
        Console.Clear()
        prepareDisplay(size)
        
        /// LRN: Pattern matching is probably one of the most powerful tools
        /// of F# and functional languages in general (Haskell has it as well).
        /// It's like a more powerful version of a switch. A C++ or C# switch requires
        /// a constant value in each case. In pattern matching each case is "a shape"
        /// you expect the matching value to have. You'll see several examples through
        /// the code.
        ///
        /// Discriminated Unions, beside Unions, implement design patterns like State Pattern,
        /// and makes handling different states of an object extremely easy and compile-safe to handle.
        /// Look the python version of this code to see how to implement it imperatively. Personally,
        /// I prefer the pattern matching form since it makes everything stronly typed.
        ///
        /// Protip: It is not being used in this project, but you can make your own custom
        /// pattern matching with, for instance, strings, ints, and other objects. Check google!
        match gameState.GameStatus with
        | Finished(player) -> gameState
        | InProgress ->
            match gameState.CurrentPhase with
            // LRN: A case in a pattern matching can have a type, and that type bounds
            // to the name of the variable you define then declaring the type of the Union
            | PlaceRing(ringsPlaced) ->
                writeAt (2, size.Height - 10) ConsoleColor.DarkRed "Please, write the two coordinates of the intersection you want to place a ring, separated by space: "
                let newGameState = playRing gameState ringsPlaced (askForPosition())
                play newGameState
            | PlaceToken ->
                let ringPositions = Board.findRingsInBoard gameState.Board gameState.Active
                printfn "In which ring do you want to play a Token? Positions: %A" (ringPositions |> List.map(fun p -> p.ToString()))
                let pos = askForPosition()
                let newGameState = playPlaceToken gameState pos
                play newGameState
            | MoveRing(ringIntersection) ->

                let validRingMoves = Board.getValidMoves gameState.Board ringIntersection.Position
                let _, validMoves = validRingMoves |> List.unzip

                printfn "Please choose any of the following positions to place the Ring: %A" (validRingMoves |> List.map(fun p -> p.ToString())) 

                let mutable placed = false
                let mutable newRingPosition = askForPosition()

                while not placed do
                    if validMoves |> List.contains(newRingPosition) then
                        let piece = { Color = gameState.Active.Color; Type = Ring }
                        Board.putPieceOnIntersection gameState.Board ringIntersection.Position piece
                        printfn "Token placed at %s" (ringIntersection.Position.ToString())
                        placed <- true
                    else
                        printfn "Please choose one coordinate from the valid move list"
                        newRingPosition <- askForPosition()

                let newGameState = playMoveRing gameState ringIntersection newRingPosition
                play newGameState
            | RemoveRows(rowsToRemove) ->
                let newGameState = playRemoveRow gameState rowsToRemove
                play newGameState
            | RemoveRing ->
                let ringPositions = Board.findRingsInBoard gameState.Board gameState.Active
                printfn "In which ring do you want to remove? Positions: %A" (ringPositions |> List.map(fun p -> p.ToString()))
                let pos = askForPosition()
                let newGameState = playRemoveRing gameState pos
                play newGameState

    play newGame |> ignore
    0 // return an integer exit code

