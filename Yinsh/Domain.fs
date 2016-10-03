

namespace Yinshbackend
open System
open System.Collections.Generic

module Domain =

    type PlayerColor =
    | Black
    | White

    let colorStr color =
        match color with
        | Black -> "Black"
        | White -> "White"

    type Player = {
            Color: PlayerColor
            CompletedRows: int
        }

    type PieceType =
    | Ring
    | Token

    type Piece = {
            Color: PlayerColor
            Type: PieceType
        }

    type Status =
    | Empty
    | Filled of Piece

    type Direction =
    | Top
    | TopLeft
    | TopRight
    | Bottom
    | BottomLeft
    | BottomRight

    type Position = {
            Letter: string
            Number: int
        } with
        override x.ToString() = sprintf "(%s, %i)" x.Letter x.Number

    type Intersection = {
        mutable Status: Status
        Position: Position
        }

    type GameStatus =
    | InProgress
    | Finished of winner: Player

    type Phase =
    | Start of ringsPlaced: int
    | Main

    type Board = {
        Intersections: IDictionary<Position, Intersection>
        }

    type GameState = {
            Players: Player[]
            GameStatus: GameStatus
            Active: Player
            Board: Board
            CurrentPhase: Phase
        }
