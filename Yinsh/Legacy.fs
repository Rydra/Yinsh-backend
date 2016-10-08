(*
    This module contains algorithms to search solution to problems in Yinsh.
    Basically there are two problems: Searching available moves from a certain
    position and finding five in a row in the board.
*)

module Legacy

open Domain
open Yinsh
open System.Collections.Generic


//#region legacy

// This implementation is more straightforward than the BFS implementation. It basically
// Tries to follow up the rules depending on the conditions and make heavy use of recursion.
// it is not as generallistic as the BFS implementation, but it's easier to understand
let getPossibleValidCoordinates board position =

    let rec getPossibleMovesRec pos hasJumped possibleDirections foundMoves =

        match possibleDirections with
        | [] -> foundMoves
        | dir :: tail ->

            let coord = Board.getNextIntersectionInDir board pos dir 1
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
        
    getPossibleMovesRec position false Board.possibleDirections []

//#endregion