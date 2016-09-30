module BusinessRules

open BoardHelper
open Yinshbackend.Domain

// A node embeds the state of a coordinate when performing bfs,
// telling whether it has jumped and the directions it must follow
type Node = {
        Coord: Coord
        ValidDirs: Direction list
        HasJumped: bool
    }

let buildNode coord validDirs hasJumped =
    { Coord = coord; ValidDirs = validDirs; HasJumped = hasJumped }

// When you realize that actually what you are doing is trying to find solutions in a problem space...
// This is a BFS implementation of finding all the possible moves that a Ring can do
let getPossibleValidCoordinatesBFS board position =
    let getNextCoordinateInDir = getNextCoordinateInDir board

    // Only empty spaces are allowed to be part of a solution
    let validMovepoint coord =
        coord.Status = Empty 

    // Given a node, returns all the possible next states that node can go
    let getPossibleNeighbors node =
        let rec getPossibleNeighborsRec remDirs foundNeighs =
            match remDirs with
            | [] -> foundNeighs
            | dir :: tail ->
                let nextCoord = getNextCoordinateInDir node.Coord.Position dir
                match nextCoord with
                | None -> getPossibleNeighborsRec tail foundNeighs // Invalid coordinate, proceed
                | Some nCoord ->
                    // Now we need to consider a few things depending on conditions for the neighbor we are
                    // analyzing. It is not a valid neighbor if the coordinate is occupied by a ring, 
                    // or if the previous node has jumped and the new neighbor is an empty space
                    let updatedNeighs =
                        match nCoord.Status with
                        | Empty when node.Coord.Status = Empty && node.HasJumped -> 
                            foundNeighs
                        | Filled(p) when p.Type = Ring -> foundNeighs
                        | Filled(p) when p.Type = Token ->
                            // Valid neighbor, but we need to mark it as a jump
                            let neighborNode = { Coord = nCoord; ValidDirs = [dir]; HasJumped = true }
                            neighborNode :: foundNeighs
                        | Empty -> 
                            // Valid neighbor. It is possible we find an empty space while jumping or not,
                            // so we need to keep track of it
                            let neighborNode = { Coord = nCoord; ValidDirs = [dir]; HasJumped = node.HasJumped }
                            neighborNode :: foundNeighs
                        | _ -> failwith "Unconsidered situation!"

                    getPossibleNeighborsRec tail updatedNeighs

        getPossibleNeighborsRec node.ValidDirs []


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
                    if validMovepoint n.Coord then Some n.Coord.Position  else None)

            let newFoundMoves = validMovepoints @ foundMoves

            getPossibleMovesRec updatedFrontier newFoundMoves

    getPossibleMovesRec [(buildNode (findCoordInBoard board position) possibleDirections false)] []
     

// This implementation is more straightforward than the BFS implementation. It basically
// Tries to follow up the rules depending on the conditions and make heavy use of recursion.
// it is not as generallistic as the BFS implementation, but it's easier to understand
let getPossibleValidCoordinates board position =

    let rec getPossibleMovesRec pos hasJumped possibleDirections foundMoves =

        match possibleDirections with
        | [] -> foundMoves
        | dir :: tail ->

            let coord = getNextCoordinateInDir board pos dir
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