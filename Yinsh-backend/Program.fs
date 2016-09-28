// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Yinshbackend.Domain

[<EntryPoint>]
let main argv = 
    getPossibleMoves "C" 1
    printfn "%A" argv
    0 // return an integer exit code

