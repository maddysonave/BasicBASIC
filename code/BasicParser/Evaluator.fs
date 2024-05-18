module Evaluator

open Parser
open System.Collections.Generic

type Environment = Dictionary<string, int>

let rec evaluate (env: Environment) (e: Expr) : string =
    match e with
    | Bstring(s) -> s
    | Num(n) -> string n
    | Bbool(b) -> if b then "true" else "false"
    | Plus(e1, e2) -> 
        let v1 = evaluate env e1 |> int
        let v2 = evaluate env e2 |> int
        string (v1 + v2)
    | Minus(e1, e2) -> 
        let v1 = evaluate env e1 |> int
        let v2 = evaluate env e2 |> int
        string (v1 - v2)
    | Times(e1, e2) -> 
        let v1 = evaluate env e1 |> int
        let v2 = evaluate env e2 |> int
        string (v1 * v2)
    | Divide(e1, e2) -> 
        let v1 = evaluate env e1 |> int
        let v2 = evaluate env e2 |> int
        if v2 = 0 then failwith "Division by zero"
        else string (v1 / v2)
    | Exp(e1, e2) -> 
        let baseValue = evaluate env e1 |> int
        let exponentValue = evaluate env e2 |> int
        string (pown baseValue exponentValue)  // power function for exponentiation
    | Paren(e) -> evaluate env e
    | Print(e) ->
        let result = evaluate env e
        printfn "%s" result
        result  // return the result after printing
    | Var(name) ->
        if env.ContainsKey(name) then
            string (env.[name])
        else
            failwithf "Variable %s not defined" name
    | Assignment(name, expr) ->
        let value = evaluate env expr |> int
        env.[name] <- value
        string value
    | Statements(stmts) ->
        let results = List.map (evaluate env) stmts
        List.last results
    | IfThen(cond, thenExpr) ->
        let condValue = evaluate env cond |> bool.Parse
        if condValue then
            evaluate env thenExpr
        else
            ""
    | IfThenElse(cond, thenExpr, elseExpr) ->
        let condValue = evaluate env cond |> bool.Parse
        if condValue then
            evaluate env thenExpr
        else
            evaluate env elseExpr

let evaluateProgram (e: Expr) : string =
    let env = Environment()
    evaluate env e
