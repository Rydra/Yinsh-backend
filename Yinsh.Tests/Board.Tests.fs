module Board.Tests

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote

open GameHelper
open Domain
open Yinsh

[<Test>]
let ``should create a new game``() =
    let game = newGame()
    game.Active.Color |> should equal White

[<Test>]
let ``initialize empty board test``() =
    let board = Board.initializeBoard()
    board.Intersections.Values |> Seq.forall(fun i -> i.Status = Empty) |> should equal true

[<Test>]
let ``should place a ring in the defined position``() =
    let board = Board.initializeBoard()
    Board.putPieceOnIntersection board ({ Letter = "A"; Number = 5 }) { Color = White; Type = Ring }
    let piece = 
        match board.Intersections.[{ Letter = "A"; Number = 5 }].Status with
        | Filled(p) -> p
        | _ -> failwith "No piece on that position"

    piece |> should equal { Color = White; Type = Ring }

[<Test>]
let ``should flip a Token in the specified position``() =
    let board = Board.initializeBoard()
    Board.putPieceOnIntersection board ({ Letter = "A"; Number = 5 }) { Color = White; Type = Token }
    Board.flipToken board { Letter = "A"; Number = 5 }
    let piece = 
        match board.Intersections.[{ Letter = "A"; Number = 5 }].Status with
        | Filled(p) -> p
        | _ -> failwith "The intersection is empty!"

    piece |> should equal { Color = Black; Type = Token }

[]