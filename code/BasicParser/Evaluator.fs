module Evaluator

open Parser
open System.Collections.Generic

// New Value type allows environment to store multiple different types of values (as opposed to just ints or strings)
type Value =
    | IntVal of int
    | BoolVal of bool
    | StringVal of string

type Environment = Dictionary<string, Value>


let rec evaluate (env: Environment) (e: Expr) : string =
    match e with
    | Bstring(s) -> s
    | Num(n) -> string n
    | Bbool(b) -> string b
    | Plus(e1, e2) -> 
        let v1 = evaluate env e1
        let v2 = evaluate env e2
        string (int v1 + int v2)
    | Minus(e1, e2) -> 
        let v1 = evaluate env e1
        let v2 = evaluate env e2
        string (int v1 - int v2)
    | Times(e1, e2) -> 
        let v1 = evaluate env e1
        let v2 = evaluate env e2
        string (int v1 * int v2)
    | Divide(e1, e2) -> 
        let v1 = evaluate env e1
        let v2 = evaluate env e2
        if int v2 = 0 then failwith "Division by zero"
        else string (int v1 / int v2)
    | Exp(e1, e2) -> 
        let baseValue = evaluate env e1
        let exponentValue = evaluate env e2
        string (pown (int baseValue) (int exponentValue))  // power function for exponentiation
    | Paren(e) -> evaluate env e
    | Print(e) ->
        let result = evaluate env e
        printfn "%s" result
        result  // return the result after printing
    | Var(name) ->
        if env.ContainsKey(name) then
            match env.[name] with
            | IntVal n -> string n
            | BoolVal b -> string b
            | StringVal s -> s
        else
            failwithf "Variable %s not defined" name
    | Assignment(name, expr) ->
        let value = 
            match expr with
            | Num n -> IntVal n
            | Bbool b -> BoolVal b
            | Bstring s -> StringVal s
            | _ -> IntVal (evaluate env expr |> int)  // default case, convert expression result to IntVal
        env.[name] <- value
        match value with
        | IntVal n -> string n
        | BoolVal b -> string b
        | StringVal s -> s
    | Statements(stmts) ->
        let results = List.map (evaluate env) stmts
        List.last results
    | IfThen (cond, thenExpr) ->
        let condValue = evaluate env cond
        match System.Boolean.TryParse(condValue) with
        | (true, true) -> evaluate env thenExpr
        | (true, false) -> ""
        | _ -> failwith "Condition must evaluate to a boolean value"
    | IfThenElse (cond, thenExpr, elseExpr) ->
        let condValue = evaluate env cond
        match System.Boolean.TryParse(condValue) with
        | (true, true) -> evaluate env thenExpr
        | (true, false) -> evaluate env elseExpr
        | _ -> failwith "Condition must evaluate to a boolean value"



    // | IfThenElse (cond, thenExpr, elseExpr) ->
    //     let condValue =
    //         match evaluate env cond with
    //         | true -> Bbool true
    //         | false -> Bbool false
    //     if condValue then
    //         evaluate env thenExpr
    //     else
    //         evaluate env elseExpr
    // | IfThenElse(cond, thenExpr, elseExpr) ->
    //     let condValue = 
    //         match evaluate env cond with
    //         | "true" -> true
    //         | "false" -> false
    //         | _ -> failwith "Invalid boolean value"
    //     if condValue then
    //         evaluate env thenExpr
    //     else
    //         evaluate env elseExpr


let evaluateProgram (e: Expr) : string =
    let env = Environment()
    evaluate env e
