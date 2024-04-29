module Parser

open Combinator

type Expr = 
| Bstring of string 
| Print of Expr

// parsing a string 
let expr, exprImpl = recparser()
let inside = (pletter |>> fun c -> string c) <|> (pdigit |>> fun c -> string c) <|> (pws1  |>>  stringify)

let rec stringBuilder (sl: string list): string  = 
    match sl with
    | [] -> ""
    | x::xs -> x + stringBuilder xs

let inside2 = pmany1 inside |>> stringBuilder 

let pbstring = 
        pbetween 
            (pchar '"')
            (inside2) 
            (pchar '"') |>> (fun s -> Bstring(s))

// printing 
let pbprint =
    pright (pstr "PRINT ") (expr) |>> (fun e -> Print(e))

exprImpl := pbstring <|> pbprint