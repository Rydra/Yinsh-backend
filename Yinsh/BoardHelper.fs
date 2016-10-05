module BoardHelper

open Domain

let letterOrders = ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"]

/// This range defines the valid coordinates of a Yinsh board.
/// Yinsh board is quite weird in terms of shape and coords, so we define,
/// for every letter coordinate, the valid number coordinates that may result
///
/// LRN: You can define dictionaries with a list of tuples
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
    let intersections = 
        [ for KeyValue (letter, lst) in validRanges do
            for num in lst do
                let pos = { Letter = letter; Number = num }
                yield (pos, { Status = Empty; Position = pos })
        ]

    { Intersections = intersections |> dict }

let findIntersectionInBoard board pos =
    let found, intersection = board.Intersections.TryGetValue(pos)
    if found then Some intersection else None

let findLetterIndex (letter:string) =
    letterOrders |> List.findIndex (fun x -> x = letter)

let getLetterByIndex idx =
    if idx < 0 || idx > (letterOrders |> List.length) - 1 then None
    else Some (letterOrders.[idx])

let validateNum n =
    if n < 1 || n > 11 then None
    else Some n

// Given a position and a direction, get the next coordinate. If the
// CoordgetNextIntersectionInDir a Yinsh board, this function returns None
let getNextIntersectionInDir board pos dir i =
    let num = pos.Number
    let letter = pos.Letter

    // obtain the next progression given a direction
    let nextnum =
        match dir with
        | Top | TopRight -> num + i |> validateNum
        | TopLeft | BottomRight -> num |> validateNum
        | Bottom | BottomLeft -> num - i |> validateNum

    let nextLetter : string option =
        match dir with
        | Top | Bottom -> Some(letter)
        | TopLeft | BottomLeft -> (letter |> findLetterIndex) - i |> getLetterByIndex
        | TopRight | BottomRight -> letter |> findLetterIndex |> (+) i |> getLetterByIndex

    // Validate the coordinate and recurse
    match (nextnum, nextLetter) with
    | (Some nn, Some nl) -> 
        let newPos = { Letter = nl; Number = nn }
        findIntersectionInBoard board newPos
    | _ -> None

let putPieceOnIntersection board pos piece =
    match findIntersectionInBoard board pos with
    | Some intersection -> intersection.Status <- Filled(piece)
    | None -> ()

let emptyIntersection board pos =
    match findIntersectionInBoard board pos with
    | Some intersection -> intersection.Status <- Empty
    | None -> ()

let flipToken board pos =
    match findIntersectionInBoard board pos with
    | Some intersection -> 
        match intersection.Status with
        | Filled(p) when p.Type = Token -> intersection.Status <- Filled({ p with Color = invertColor p.Color })
        | _ -> ()
    | None -> ()