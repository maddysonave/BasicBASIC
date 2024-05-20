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
    | Keyword of string
    | Print of Expr
    | Paren of Expr       // for expressions within parentheses
    // Variable assignment
    | Assignment of string * Expr
    // Statement list to handle multiple lines
    | Statements of Expr list
    // Conditionals
    | IfThen of Expr * Expr
    | IfThenElse of Expr * Expr * Expr

(* END AST DEFINITION *)

(* START PARSER DEFINITION *)
// recursive parsers for different levels of precedence
let expr, exprImpl = recparser()
let factorExpr, factorExprImpl = recparser()
let expExpr, expExprImpl = recparser()

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

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Parsing a variable vs. keywords
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

let pvar =
    pseq pletter (pmany0 (pletter <|> pdigit))
        (fun (c, cs) -> Var (string c + stringify cs))


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Parsing a variable assignment
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

let assignment =
    pseq pvar (pright (pright pws0 (pchar '=')) (pright pws0 expr))
        (fun (Var v, e) -> Assignment (v, e))

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Loops, Conditionals, Functions
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// parser for print statements
let pbprint =
    pright (pstr "PRINT ") (expr) |>> (fun e -> Print(e))

// parser for boolean values
let pbool = 
    (pstr "true" |>> (fun _ -> Bbool true)) <|> 
    (pstr "false" |>> (fun _ -> Bbool false))

let pkeyword (keyword: string) =
    pleft (pstr keyword) pws0

let pif = pkeyword "IF"
let pthen = pkeyword "THEN"
let pelse = pkeyword "ELSE"
let pgoto = pkeyword "GO TO"
 
let keywords = pif <|> pthen <|> pelse <|> pgoto

let keywordExpr = keywords |>> Keyword

// let ifThenElse =
//     pright (pright pws0 (pstr "IF")) (
//         pseq expr (
//             pright pws0 (
//                 pright (pstr "THEN") (
//                     pseq expr (
//                         pright pws0 (
//                             pright (pstr "ELSE") expr
//                         ) (fun (e3) -> e3)
//                     ) (fun (e2, e3) -> (e2, e3))
//                 )
//             )
//         ) (fun (cond, (thenExpr, elseExpr)) -> IfThenElse (cond, thenExpr, elseExpr))
//     )

// basic expression: numbers, strings, print statements, booleans, and parentheses
let atom =
    num <|> 
    pbstring <|> 
    pbprint <|>
    pbool <|>
    (pbetween (pleft (pchar '(') pws0) expr (pright pws0 (pchar ')')) |>> Paren) <|>
    pvar

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
                Exp(e1, acc)  // construct the expression with right associativity
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
/// Multi-Line Support
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Parsing a single line (expression or assignment or conditionals)
let line : Parser<Expr> =
    keywordExpr <|> assignment  <|> expr //ifThenElse <|>

// Parsing multiple lines
let lines : Parser<Expr list> =
    pmany1 (pleft line (pmany0 (psat (fun c -> c = '\n' || is_whitespace c))))

// Combining into a single program (statements)
let program : Parser<Expr> =
    lines |>> Statements

// parser entry point: ensures entire input is consumed
let grammar = pleft program peof

// parse function to run the parser on input and handle the outcome
let parse input =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_, _) -> None
