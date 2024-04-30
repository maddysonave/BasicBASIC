module Parser
open Combinator

(* START AST DEFINITION *)
type Expr = 
| Bstring of string 
| Print of Expr
(* END AST DEFINITION *)

(* START PARSER DEFINITION *)
// parsing a string 
let expr, exprImpl = recparser()
let inside = (pletter |>> fun c -> string c) <|> (pdigit |>> fun c -> string c) <|> (pws1  |>>  stringify)

// concatenates a string list into one string
let rec stringBuilder (sl: string list): string  = 
    match sl with
    | [] -> ""
    | x::xs -> x + stringBuilder xs

let inside2 = (pmany1 inside) |>> stringBuilder 

let pbstring = 
    pbetween (pchar '"') (inside2) (pchar '"') |>> (fun s -> Bstring(s)) 

// printing 
let pbprint =
    pright (pstr "PRINT ") (expr) |>> (fun e -> Print(e))

exprImpl := pbstring <|> pbprint

let grammar = pleft expr peof
(* END PARSER DEFINITION *)

let parse input =
    let i = prepare input
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_) -> None