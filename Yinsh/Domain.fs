

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
        }

    type Coord = {
        mutable Status: Status
        Position: Position
        }

    type Board = {
        CoordList: Coord list
        }