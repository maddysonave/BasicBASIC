module Parser
open Combinator


(* START AST DEFINITION *)
type Expr =
// Primitives
| Bstring of string
| Num of int
| Bbool of bool
// Arithmetic operators
| Plus of Expr * Expr
| Minus of Expr * Expr
| Times of Expr * Expr
| Divide of Expr * Expr
| Exp of Expr * Expr  // for exponentiation
// Other things 
| Var of string
| Print of Expr
| Paren of Expr       // for expressions within parentheses

// type IfExpr = 
// | If of Expr * Expr * Expr
// | True
// | False

// type ForLoop = 
// | ForNext of string * Expr * Expr * Expr * Expr

// also want to add a declaration parser 

(* END AST DEFINITION *)


(* START PARSER DEFINITION *)
// recursive parsers for different levels of precedence
let expr, exprImpl = recparser()
let factorExpr, factorExprImpl = recparser()
let expExpr, expExprImpl = recparser()

// insert parser for if then loop
// insert parser for for next loop

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Primitives
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

// parser for numbers
let num = 
    pmany1 pdigit |>> (fun ds -> 
        let s = stringify ds
        let n = int s
        Num(n))

// parser for strings
let inside = 
    (pletter |>> fun c -> string c) <|> (pdigit |>> fun c -> string c) <|> (pws1  |>>  stringify)

// concatenates a string list into one string
let rec stringBuilder (sl: string list): string  = 
    match sl with
    | [] -> ""
    | x::xs -> x + stringBuilder xs

let inside2 = (pmany1 inside) |>> stringBuilder 

let pbstring = 
    pbetween (pchar '"') (inside2) (pchar '"') |>> (fun s -> Bstring(s))

// boolean parser
// let pbbool : Parser<Expr> =
//     (pstr "true" |>> (fun _ -> Bbool true))
//     <|> (pstr "false" |>> (fun _ -> Bbool false))

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Parsing a varible
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

// REFERENCE CODE

// let pVar : Parser<Expr> =
//     pstring "var" |> pbind (fun varName ->
//         pmany0 (pletter <|> pdigit) |>> (fun restChars ->
//             Var (varName + String(restChars |> List.toArray))
//         )
//     )

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Loops, Conditionals, Functions
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

// parser for print statements
let pbprint =
    pright (pstr "PRINT ") (expr) |>> (fun e -> Print(e))

// basic expression: numbers, strings, print statements, and parentheses
let atom =
    num <|> 
    pbstring <|> 
    pbprint <|>
    (pbetween (pleft (pchar '(') pws0) expr (pright pws0 (pchar ')')) |>> Paren)


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Arithmetic Operations
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

// exponentiation parser: highest precedence, right associative
let exponentiationExpr =
    pseq atom (pmany0 (
        pright pws0 (
            pseq (pchar '^') (pright pws0 atom) (fun (op, expr) -> (op, expr))
        )
    ))
        (fun (e1, ops) ->
            List.foldBack (fun (op, e2) acc ->
                Exp(e2, acc)  // construct the expression with right associativity
            ) ops e1
        )
expExprImpl := exponentiationExpr <|> atom

// multiplication and division parser: higher precedence than addition and subtraction
let multDivExpr =
    pseq expExpr (pmany0 (
        pright pws0 (
            pseq (pchar '*' <|> pchar '/') (pright pws0 expExpr)
                  (fun (op, expr) -> (op, expr))  // create a tuple (operator, expression)
        )
    ))
        (fun (e1, ops) ->
            List.fold (fun acc (op, e2) ->
                match op with
                | '*' -> Times(acc, e2)  // multiply the accumulator with the new expression
                | '/' -> Divide(acc, e2)  // divide the accumulator by the new expression
                | _ -> failwith "Unexpected operator"
            ) e1 ops
        )
factorExprImpl := multDivExpr <|> expExpr

// addition and subtraction parser: lowest precedence
let addSubExpr =
    pseq factorExpr (pmany0 (
        pright pws0 (
            pseq (pchar '+' <|> pchar '-') (pright pws0 factorExpr)
                  (fun (op, expr) -> (op, expr))  // create a tuple (operator, expression)
        )
    ))
        (fun (e1, ops) ->
            List.fold (fun acc (op, e2) ->
                match op with
                | '+' -> Plus(acc, e2)  // add the accumulator to the new expression
                | '-' -> Minus(acc, e2)  // subtract the new expression from the accumulator
                | _ -> failwith "Unexpected operator"
            ) e1 ops
        )
exprImpl := addSubExpr

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Data structures
//////////////////////////////////////////////////////////////////////////////////////////////////////////////



// parser entry point: ensures entire input is consumed
let grammar = pleft expr peof

// parse function to run the parser on input and handle the outcome
let parse input =
    let i = prepare input
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_) -> None
