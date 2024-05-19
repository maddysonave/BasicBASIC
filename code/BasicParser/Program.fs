open Parser
open Evaluator
open Combinator
open System.IO
open System.Collections.Generic

let readLinesFromFile path = File.ReadAllLines(path)

[<EntryPoint>]
let main args =
    // checking for file
    if args.Length = 0 || not (File.Exists(args.[0])) then
        printfn "Usage: dotnet run <filename.txt>"
        1
    else
        let file = readLinesFromFile args.[0]
        // concatenate all lines to form the input for the parser
        let input = String.concat "\n" file
        let asto = parse input
        match asto with
        | Some ast -> 
            let env = Dictionary<string, Value>()  // create a new environment
            evaluate env ast |> ignore
            0
        | None -> 
            printfn "Invalid program."
            1
