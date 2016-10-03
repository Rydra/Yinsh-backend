module BusinessRules

open BoardHelper
open Yinshbackend.Domain

// A node embeds the state of a coordinate when performing bfs,
// telling whether it has jumped and the directions it must follow
type Node = {
        Intersection: Intersection
        AllowedDirs: Direction list
        HasJumped: bool
    }

let buildNode intersection validDirs hasJumped =
    { Intersection = intersection; AllowedDirs = validDirs; HasJumped = hasJumped }

// When you realize that actually what you are doing is trying to find solutions in a problem space...
// This is a BFS implementation of finding all the possible moves that a Ring can do
let getValidMoves board position =
    let getNextIntersectionInDir = getNextIntersectionInDir board

    // Only empty spaces are allowed to be part of a solution
    let validMovepoint intersection =
        intersection.Status = Empty 

    // Given a node, returns all the possible next states that node can go
    let getPossibleNeighbors node =
        let rec getPossibleNeighborsRec remDirs foundNeighs =
            match remDirs with
            | [] -> foundNeighs
            | dir :: tail ->
                let nextIntersection = getNextIntersectionInDir node.Intersection.Position dir 1
                match nextIntersection with
                | None -> getPossibleNeighborsRec tail foundNeighs // Invalid coordinate, proceed
                | Some intersection ->
                    // Now we need to consider a few things depending on conditions for the neighbor we are
                    // analyzing. It is not a valid neighbor if the coordinate is occupied by a ring, 
                    // or if the previous node has jumped and the new neighbor is an empty space
                    let updatedNeighs =
                        match intersection.Status with
                        | Empty when node.Intersection.Status = Empty && node.HasJumped -> 
                            foundNeighs
                        | Filled(p) when p.Type = Ring -> foundNeighs
                        | Filled(p) when p.Type = Token ->
                            // Valid neighbor, but we need to mark it as a jump
                            let neighborNode = { Intersection = intersection; AllowedDirs = [dir]; HasJumped = true }
                            neighborNode :: foundNeighs
                        | Empty -> 
                            // Valid neighbor. It is possible we find an empty space while jumping or not,
                            // so we need to keep track of it
                            let neighborNode = { Intersection = intersection; AllowedDirs = [dir]; HasJumped = node.HasJumped }
                            neighborNode :: foundNeighs
                        | _ -> failwith "Unconsidered situation!"

                    getPossibleNeighborsRec tail updatedNeighs

        getPossibleNeighborsRec node.AllowedDirs []


    // Although we are using a BFS-ish way to implement this, the frontier does not need to be
    // a FIFO list since we are not looking for just 1 solution, but every one of them.
    // also we do not need a closed/explored list since we moving outside of the initial
    // node we are explicitly restricting the directions, so no possible wait to revisit
    // a node
    let rec getPossibleMovesRec frontier foundMoves =

        match frontier with
        | [] -> foundMoves
        | node :: tail ->

            let neighbors = getPossibleNeighbors node
            let updatedFrontier = neighbors @ tail
            
            let validMovepoints = 
                neighbors 
                |> List.choose(fun n ->
                    if validMovepoint n.Intersection then Some n.Intersection.Position else None)

            let newFoundMoves = validMovepoints @ foundMoves

            getPossibleMovesRec updatedFrontier newFoundMoves

    let startingIntersection = (findIntersectionInBoard board position)
    let startNode = buildNode (startingIntersection |> Option.get) possibleDirections false
    getPossibleMovesRec [startNode] []
     

// This implementation is more straightforward than the BFS implementation. It basically
// Tries to follow up the rules depending on the conditions and make heavy use of recursion.
// it is not as generallistic as the BFS implementation, but it's easier to understand
let getPossibleValidCoordinates board position =

    let rec getPossibleMovesRec pos hasJumped possibleDirections foundMoves =

        match possibleDirections with
        | [] -> foundMoves
        | dir :: tail ->

            let coord = getNextIntersectionInDir board pos dir 1
            match coord with
            | Some c -> 

                // Decide whether this neighbor can actually be declared as a solution
                // (Only include empty spaces). If it does, add it as a possible solution
                let updatedAcc = 
                    match c.Status with
                    | Empty -> c.Position :: foundMoves
                    | _ -> foundMoves

                // Now we need to consider a few things depending on conditions for the position
                // we are analyzing. 
                // 1- If the new position is filled with a Ring, we cannot move through there
                // and we must continue recursing in a different directions
                // 2- If the new position is empty, but we have jumped, we need to add this point as valid
                // coordinate, but stop following that direction
                // 3- If the coordinate is empty, add the point to the list of found points and recurse
                // in that direction to find more points
                // 4- If the coordinate is a token, we don't have to add the point, but recurse until finding an
                // empty space
                match c.Status with
                | Filled(p) when p.Type = Ring -> 
                    getPossibleMovesRec pos hasJumped tail updatedAcc
                | Empty when hasJumped -> 
                    getPossibleMovesRec pos hasJumped tail updatedAcc
                | Empty ->
                    let additionalPosInDirection = getPossibleMovesRec c.Position false [dir] []
                    getPossibleMovesRec pos hasJumped tail (updatedAcc @ additionalPosInDirection)
                | Filled(p) when p.Type = Token ->
                    let additionalPosInDirection = getPossibleMovesRec c.Position true [dir] []
                    getPossibleMovesRec pos hasJumped tail (updatedAcc @ additionalPosInDirection)
                | _ -> failwith "Inconsidered situation!"
            | None -> getPossibleMovesRec pos hasJumped tail foundMoves
        
    getPossibleMovesRec position false possibleDirections []


// Takes a point and looks 5 moves ahead in a direction to assess whether a row exists.
// If it does, returns it. Otherwise, returns None.
let fiveInARowExists board intersection dir =

    match intersection.Status with
    | Empty -> (false, [])
    | Filled(piece) ->

        let condition intersection =
            match intersection with
            | Some it ->
                match it.Status with
                | Filled(p) when p.Color = piece.Color -> p.Type = Token
                | _ -> false
            | None -> false

        // We need to check 5 positions in that direction, and process while the piece is a
        // Token and the color is the same. If we took the whole 5 intersections, then we found
        // a five in a row!
        let analyzedIntersections = 
            [0 .. 4] 
            |> Seq.map(fun x -> getNextIntersectionInDir board intersection.Position dir x)
            |> Seq.takeWhile condition
            |> Seq.toList

        // Even if we haven't found a five in a row, returning the list of analyzed intersections
        // Will allow the caller to add it to an ignore list so that we don't do redundant processing
        (analyzedIntersections |> List.length |> (=) 5, analyzedIntersections |> List.choose id |> List.map (fun x -> x.Position))

open System.Collections.Generic

let findFiveInARow board =
    //  Preconditions:
    // 1- a Five in a row means there are 5 token of the same color in a single direction
    // 2- Rows can overlap. if there are 6 in a row, we must detect in this situation 2 5 in a row

    // Create an ignore list. This list tells, for certain positions, which directions can be
    // ignored, so that you do not look twice for a five in a row or follow useless paths
    let ignoreList = Dictionary<Position, List<Direction>>() 

    // Brute force: Look every intersection with a token and assess if row exists
    let intersectionsWithTokens = 
        board.Intersections.Values
        |> List.ofSeq
        |> List.filter (fun x ->
                match x.Status with
                | Filled(p) -> p.Type = Token
                | _ -> false)

    let rec findFiveInRowRec intersectionsWithTokens foundRows =
        match intersectionsWithTokens with
        | [] -> foundRows
        | intersection :: tail ->
            let dirsToIgnore = 
                if ignoreList.ContainsKey(intersection.Position) then
                    ignoreList.[intersection.Position]
                else List<Direction>()
            
            let fiveInRows = 
                [Direction.Top; Direction.TopLeft; Direction.TopRight]
                |> List.except(dirsToIgnore)
                |> List.choose(fun dir -> 
                    let f, analyzedRows = fiveInARowExists board intersection dir
                    if f then Some analyzedRows
                    else 
                        // No five in a row found, 
                        // we can safely ignore for every analyzed token in a certain direction.
                        // Pointless to follow them in the same direction if we haven't already found any row
                        for r in analyzedRows do
                            if not (ignoreList.ContainsKey(r)) then
                                ignoreList.Add(r, List())
                            ignoreList.[r].Add(dir)
                        None
                    )

            findFiveInRowRec tail (foundRows @ fiveInRows)
    
    findFiveInRowRec intersectionsWithTokens []


open Priority_Queue

type PathNode =
    {
        F: int
        G: int
        H: int
        Position: Position
        PreviousNode : PathNode option
    }

let createPathNode f g h pos =
    { F = f; G = g; H = h; Position = pos; PreviousNode = None }

let getPath board startPos endPos =

    // We'll use Manhattan distance
    let heuristic position =
        let mapLetterToNumber =
            function
            | "A" -> 1
            | "B" -> 2
            | "C" -> 3
            | "D" -> 4
            | "E" -> 5
            | "F" -> 6
            | "G" -> 7
            | "H" -> 8
            | "I" -> 9
            | "J" -> 10
            | "K" -> 11
            | _ -> failwith "unrecognized Letter"

        let c1 = mapLetterToNumber position.Letter
        let c2 = position.Number

        c1 - (mapLetterToNumber endPos.Letter) + (c2 - endPos.Number)
            

    // Given a node, returns all the possible next states that node can go
    let getPossibleNeighbors pos =
        let rec getPossibleNeighborsRec remDirs foundNeighs =
            match remDirs with
            | [] -> foundNeighs
            | dir :: tail ->
                let nextIntersection = getNextIntersectionInDir board pos dir 1
                match nextIntersection with
                | None -> getPossibleNeighborsRec tail foundNeighs // Invalid coordinate, proceed
                | Some intersection -> getPossibleNeighborsRec tail (intersection.Position :: foundNeighs)

        getPossibleNeighborsRec possibleDirections []

    let startNode = createPathNode 0 0 0 startPos

    let openList = SimplePriorityQueue<PathNode>()
    openList.Enqueue(startNode, float32 startNode.F)

    let reconstructPath pathNode =
        []

    let mutable explored = HashSet<Position>()
    let mutable path = []
    //while openList.Count > 0 do
    //    let node = openList.Dequeue()
    //    if node.Position = endPos then
    //        path <- reconstructPath node
    //    
    //    else
    //        let neighbors = getPossibleNeighbors pos
    //        ()

    path