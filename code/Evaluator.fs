module Evaluator
open Parser

let rec prettyprint (e : Expr) : string = 
    match e with 
    | Bstring(s) -> '"'+ s.ToString() + '"'
    | Print(e) -> "PRINT '" + prettyprint e + "'"