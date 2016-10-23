module GameHelper

open Domain
open Yinsh

let newGame() =
    let whitePlayer = { Color = White; CompletedRows = 0 }
    let blackPlayer = { Color = Black; CompletedRows = 0 }
    let players = [|whitePlayer; blackPlayer|]
    let gameStatus = InProgress
    let board = Board.initializeBoard()
    let currentPhase = PlaceRing { RingsPlaced = 0 }
    { Players = players; Active = whitePlayer; GameStatus = gameStatus; Board = board; CurrentAction = currentPhase }