open Parser
open Evaluator
open Combinator
open System.IO

let readLinesFromFile path = File.ReadAllLines(path)

[<EntryPoint>]
let main args =
    // checking for file
    if args.Length = 0 || not (File.Exists(args.[0])) then
        printfn "Usage: dotnet run <filename.txt>"
        1
    else
        let file = readLinesFromFile args[0]
        // for specification 1.0, the language only accepts 1 line at a time
        let input = file[0]
        let asto = parse input
        match asto with
        | Some ast -> printfn "%A" (prettyprint ast)
        | None -> printfn "Invalid program."
        0