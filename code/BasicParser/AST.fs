module AST
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
    // Variable assignment
    | Assignment of string * Expr
    // Statement list to handle multiple lines
    | Statements of Expr list
    // Conditionals
    | IfThen of Expr * Expr
    | IfThenElse of Expr * Expr * Expr

(* END AST DEFINITION *)
