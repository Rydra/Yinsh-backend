module BoardHelper

open Yinshbackend.Domain

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

let possibleDirections = [Top; TopLeft; TopRight; Bottom; BottomLeft; BottomRight]

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
let initializeBoard() : Board =
    let coordinates = 
        [ for KeyValue (letter, lst) in validRanges do
            for num in lst do
                yield { Status = Empty; Position = { Letter = letter; Number = num } }
        ]

    { CoordList = coordinates }

let findCoordInBoard board pos =
    board.CoordList |> List.find(fun x -> x.Position = pos)

let findLetterIndex (letter:string) =
    letterOrders |> List.findIndex (fun x -> x = letter)

let getLetterByIndex idx =
    if idx < 0 || idx > (letterOrders |> List.length) - 1 then None
    else Some (letterOrders.[idx])

let validateNum n =
    if n < 1 || n > 11 then None
    else Some n

// TODO: This check can be way further optimized.
// Tells whether a given coordinate is in the range of the board and exists
let coordinateExists position =
    validRanges.[position.Letter] |> List.exists(fun n -> n = position.Number)

// Given a position and a direction, get the next coordinate. If the
// Coordinate is not valid for a Yinsh board, this function returns None
let getNextCoordinateInDir board pos dir =
    let num = pos.Number
    let letter = pos.Letter

    // obtain the next progression given a direction
    let nextnum =
        match dir with
        | Top | TopRight -> num + 1 |> validateNum
        | TopLeft | BottomRight -> num |> validateNum
        | Bottom | BottomLeft -> num - 1 |> validateNum

    let nextLetter : string option =
        match dir with
        | Top | Bottom -> Some(letter)
        | TopLeft | BottomLeft -> (letter |> findLetterIndex) - 1 |> getLetterByIndex
        | TopRight | BottomRight -> letter |> findLetterIndex |> (+) 1 |> getLetterByIndex

    // Validate the coordinate and recurse
    match (nextnum, nextLetter) with
    | (Some nn, Some nl) -> 
        let newPos = { Letter = nl; Number = nn }
        if coordinateExists newPos then
            let coord = findCoordInBoard board newPos
            Some coord
        else None
    | _ -> None

let putPieceOnCoord board pos piece =
    let c = findCoordInBoard board pos
    c.Status <- Filled(piece)