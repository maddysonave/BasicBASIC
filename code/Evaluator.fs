module Evaluator
open Parser

let rec evaluate (e : Expr) : string =
    match e with
    | Bstring(s) -> s
    | Num(n) -> string n
    | Plus(e1, e2) -> 
        let v1 = evaluate e1 |> int
        let v2 = evaluate e2 |> int
        string (v1 + v2)
    | Minus(e1, e2) -> 
        let v1 = evaluate e1 |> int
        let v2 = evaluate e2 |> int
        string (v1 - v2)
    | Times(e1, e2) -> 
        let v1 = evaluate e1 |> int
        let v2 = evaluate e2 |> int
        string (v1 * v2)
    | Divide(e1, e2) -> 
        let v1 = evaluate e1 |> int
        let v2 = evaluate e2 |> int
        if v2 = 0 then failwith "Division by zero"
        else string (v1 / v2)
    | Exp(e1, e2) -> 
        let v1 = evaluate e1 |> int
        let v2 = evaluate e2 |> int
        string (pown v1 v2)  // power function for exponentiation
    | Paren(e) -> evaluate e
    | Print(e) ->
        let result = evaluate e
        printfn "%s" result
        result  // return the result after printing