module Yinsh.Tests

open FsUnit
open FsCheck
open NUnit.Framework
open GameHelper
open Domain
open Phases

[<Test>]
let ``create a new game test``() =
    let game = newGame()
    game.Active.Color |> should equal White

[<Test>]
let ``initialize empty board test``() =
    let board = Board.initializeBoard()
    board.Intersections.Values |> Seq.forall(fun i -> i.Status = Empty) |> should equal true

let createPiece color pieceType =
    { Type = pieceType; Color = color }

[<Test>]
let ``when playing a token on a ring, the token must be on the board``() =
    let game = newGame()
    let updatedGame = { game with CurrentAction = PlaceToken(PlaceTokenAction()) }
    updatedGame.Board |> Board.placePieceAt (Position "A" 2)  (createPiece Black Ring)
    match game.CurrentAction with
    | PlaceToken(state) ->
        let ugame = updatedGame |> state.DoAction (Position "A" 2)
        let intersection = Board.findIntersection ugame.Board (Position "A" 2)
        intersection.Value.Status |> should equal (Filled(createPiece Black Token))
        ugame.CurrentAction |> should equal (MoveRing({ RingToMove = intersection.Value }))
    | _ -> ()
