

namespace Yinshbackend
open System

module Domain =

    type PlayerColor =
    | Black
    | White

    type PieceType =
    | Ring
    | Token

    type Piece = {
            Color: PlayerColor
            Type: PieceType
        }

    type Intersection =
    | Empty
    | Filled of Piece

    type Moves =
    | Top
    | TopLeft
    | TopRight
    | Bottom
    | BottomLeft
    | BottomRight

    type Coord = {
        mutable Intersection: Intersection
        Letter: string
        Number: int
        }

    type HexBoard = {
        CoordList: Coord list
        }

    let letterOrders = ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"]

    /// This range defines the valid coordinates of a Yinsh board.
    /// Yinsh board is quite weird in terms of shape and coords, so we define,
    /// for every letter coordinate, the valid number coordinates that may result
    let validRanges = 
        dict [
                "A", [2 .. 5]
                "B", [1 .. 7]
                "C", [1 .. 8]
                "D", [1 .. 9]
                "E", [1 .. 10]
                "F", [2 .. 10]
                "G", [2 .. 11]
                "H", [3 .. 11]
                "I", [4 .. 11]
                "J", [5 .. 11]
                "K", [7 .. 10]
        ]

    /// Yinsh board is kind of weird. It looks like hexagonal,it points out 11 coordinates for X and 11 coordinates for Y,
    /// But there are a total of 85 intersections (which is far from 11x11). So this algorithm is NOT correct at all.
    /// We need to come up with a way to represent this. For instance, G11 exists, but B11 does not nor does A1
    /// (Grab a picture of Yinsh board to see it by yourself)
    /// Since the Yinsh board is so weird, let's declare an initialize function that does not take a width and height,
    /// but a number of intersections... however we still run into the problem of how to create the cells and assign a proper,
    /// valid coordinate...This makes me think I have two options... either I can hardcode the coordinates (and after that hardcoding
    /// for specific coordinates which are the valid moves, like B2, that allows for all directions except bottom-left) or
    /// try to find a suitable pattern (which will be somewhat difficult in this board)
    /// All these peculiarities and exceptions will end up becoming a source of ifs and cases, which by themselves
    /// pose a maintainability problem (we will have to think of a better alternative)
    let initializeBoard() : HexBoard =
        let coordinates = 
            [ for KeyValue (letter, lst) in validRanges do
                for num in lst do
                    yield { Intersection = Empty; Letter = letter; Number = num }
            ]

        { CoordList = coordinates }

    let possibleMoves = [Top; TopLeft; TopRight; Bottom; BottomLeft; BottomRight]

    let getPossibleMoves (letter:string) num =
        let findLetterIndex (letter:string) =
            letterOrders |> List.findIndex (fun x -> x = letter)
        let getLetterByIndex idx =
            if idx < 0 || idx > (letterOrders |> List.length) - 1 then None
            else Some (letterOrders.[idx])

        let validateNum n =
            if n < 1 || n > 11 then None
            else Some n

        // TODO: This check can be way further optimized.
        let coordinateExists (letr:string) number =
            validRanges.[letr] |> List.exists(fun n -> n = number)

        let rec getPossibleMovesRec currOptions acc =
            match currOptions with
            | [] -> acc
            | head :: tail ->
                let nextnum =
                    match head with
                    | Top | TopRight -> num + 1 |> validateNum
                    | TopLeft | BottomRight -> num |> validateNum
                    | Bottom | BottomLeft -> num - 1 |> validateNum

                let nextLetter : string option =
                    match head with
                    | Top | Bottom -> Some(letter)
                    | TopLeft | BottomLeft -> (letter |> findLetterIndex) - 1 |> getLetterByIndex
                    | TopRight | BottomRight -> letter |> findLetterIndex |> (+) 1 |> getLetterByIndex

                match (nextnum, nextLetter) with
                | (None, _)
                | (_, None) -> getPossibleMovesRec tail acc
                | (Some nn, Some nl) ->
                    if coordinateExists nl nn then
                        getPossibleMovesRec tail (head::acc)
                    else
                        getPossibleMovesRec tail acc
        
        getPossibleMovesRec possibleMoves []
     