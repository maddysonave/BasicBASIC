module Evaluator

open Parser
let rec eval ast : int =
    match ast with
    | Num(n) -> n
    | Plus(e1, e2) ->
        let e1res = eval e1
        let e2res = eval e2
        e1res + e2res