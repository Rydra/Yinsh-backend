

namespace Yinsh
open System

module Utils =
    let prompt text =
        printf text
        Console.ReadLine()

    let promptn text =
        printfn text
        Console.ReadLine()
