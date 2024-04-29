module Parser

open Combinator

type Expr = 
| Bstring of string 
| Print of Expr

// parsing a string 
let expr, exprImpl = recparser()
let inside = (pletter |>> fun c -> string c) <|> (pdigit |>> fun c -> string c) <|> (pws1  |>>  stringify)
let string = 
        pbetween 
            (pchar '"')
            (pmany1 inside) 
            (pchar '"')

// printing 
let print =
    pbetween
        (pchar '"')
        (pleft expr pws1)
        (pchar '"')

exprImpl := string <|> print