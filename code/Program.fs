open Parser
open Evaluator
open Combinator

[<EntryPoint>]

let main args =
    let input = args[0]
    let asto = parse input
    match asto with
    | Some ast -> printfn "%A" (prettyprint ast)
    | None -> printfn "Invalid program."
    0
