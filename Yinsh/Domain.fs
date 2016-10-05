(*
    This module contains all the Domain model classes
*)

module Domain

/// LRN: Domain modeling is really one of the most pleasant
/// experiences of developing in F#. It is so clear, concise and
/// succint... No need for a lot of noisy, bolderplate code
/// just to express some ideas like in C# classes and properties.

open System
open System.Collections.Generic

// Two colors: Black and White
/// LRN: This is a Discriminated Union. Discriminated Unions are a very powerful
/// characteristic of F# and functional languages in general. To compare
/// they can implement enums, or a strong-typed version of the state pattern.
/// You can even define different types for the discriminated unions. You
/// usually use them in pattern matching
type PlayerColor =
| Black
| White

// Gets the string representation of a color
let colorStr color =
    match color with
    | Black -> "Black"
    | White -> "White"

// Gets the opposite color
let invertColor color =
    match color with 
    | Black -> White
    | White -> Black

// Represents a player in the game
/// LRN: This is a record type. Record types are immutable
/// (pretty much like structs in C#) and implement member-wise
/// equality in comparison, not by reference.
type Player = {
        Color: PlayerColor
        CompletedRows: int
    }

// There are two types of pieces: Rings and token
type PieceType =
| Ring
| Token

// A piece is defined by a type and a color
type Piece = {
        Color: PlayerColor
        Type: PieceType
    }

/// This is another discriminated union, but the Filled state wraps
/// and object of type Piece
type Status =
| Empty
| Filled of Piece

// Each intersection can have up to 6 directions
type Direction =
| Top
| TopLeft
| TopRight
| Bottom
| BottomLeft
| BottomRight

// In order to have the same semantics as the game, each position
// is represented by a letter coordinate and a number coordinate
type Position = {
        Letter: string
        Number: int
    } with
    override x.ToString() = sprintf "(%s, %i)" x.Letter x.Number

// An intersection is represented by a status (Empty or Filled with a Piece)
// And a position
type Intersection = {
    mutable Status: Status
    Position: Position
    }

type GameStatus =
| InProgress
| Finished of winner: Player

// In this game there are several actions that the user can do in a certain gameState
/// LRN: Another discriminated union. Each state embeds a diferent type of object
type Phase =
| PlaceRing of ringsPlaced: int
| PlaceToken
| MoveRing of ringToMoMove: Intersection
| RemoveRows of rowsToRemove: Position list list
| RemoveRing

// A board is represented by a set of intersections. For performance
// purposes the intersections are indexed by position
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
