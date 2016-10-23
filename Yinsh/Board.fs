namespace Yinsh

module Board =

    open Domain
    open System.Collections.Generic

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

    let findIntersection board pos =
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
    // the intersection is out of the bopunds of a Yinsh board,
    // this function returns None
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
            findIntersection board newPos
        | _ -> None

    /// Puts a piece on an intersection, replacing whatever there was there.
    let placePieceAt pos piece board =
        match findIntersection board pos with
        | Some intersection -> intersection.Status <- Filled(piece)
        | None -> ()

    /// Removes any piece that may had been in the specified position
    let emptyIntersection pos board =
        match findIntersection board pos with
        | Some intersection -> intersection.Status <- Empty
        | None -> ()

    /// Flips the color of a token piece in the specified position
    let flipToken board pos =
        match findIntersection board pos with
        | Some ({Status = Filled({Type = Token} as p)} as intersection) -> 
            intersection.Status <- Filled({ p with Color = invertColor p.Color })
        | _ -> ()

    /// Find the rings in the board for the specified player
    let findRingsInBoard board (player:Player) =
        let ringPositions = 
            board.Intersections 
            |> Seq.filter(fun kvp ->
                let value = kvp.Value
                match value.Status with
                | Filled(p) when p.Color = player.Color && p.Type = Ring -> true
                | _ -> false)
            |> Seq.map(fun kvp -> kvp.Key)
            |> Seq.toList

        ringPositions

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
    let getValidMovesFrom position board =
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
                    // Now we need to consider a few things depending on conditions for the neighbor we are
                    // analyzing. It is not a valid neighbor if the coordinate is occupied by a ring, 
                    // or if the previous node has jumped and the new neighbor is an empty space
                    match nextIntersection with
                    | Some { Status = Filled({ Type = Ring }) }
                    | None // Invalid coordinate, proceed
                    | Some { Status = Empty } when node.Intersection.Status = Empty && node.HasJumped ->
                        getPossibleNeighborsRec tail foundNeighs

                    | Some ({ Status = Filled({ Type = Token }) } as intersection) ->
                        let neighborNode = { Intersection = intersection; AllowedDirs = [dir]; HasJumped = true }
                        getPossibleNeighborsRec tail (neighborNode :: foundNeighs)

                    | Some ({ Status = Empty } as intersection) ->
                        let neighborNode = { Intersection = intersection; AllowedDirs = [dir]; HasJumped = node.HasJumped }
                        getPossibleNeighborsRec tail (neighborNode :: foundNeighs)

                    | _ -> getPossibleNeighborsRec tail foundNeighs

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
            
                // Getting the valid moves as well as the direction will be useful for quick pathfinding
                let validMovepoints = 
                    neighbors 
                    |> List.choose(fun n ->
                        if validMovepoint n.Intersection then Some (n.AllowedDirs.[0], n.Intersection.Position) else None)

                let newFoundMoves = validMovepoints @ foundMoves

                getPossibleMovesRec updatedFrontier newFoundMoves

        let startingIntersection = (findIntersection board position)
        let startNode = buildNode (startingIntersection |> Option.get) possibleDirections false
        getPossibleMovesRec [startNode] []

         
    // Takes a point and looks 5 moves ahead in a direction to assess whether a row exists.
    // If it does, returns it. Otherwise, returns None.
    let private fiveInARowExists board intersection dir =

        match intersection.Status with
        | Empty -> (false, []) // Extra check
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