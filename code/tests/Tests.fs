namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Combinator
open Evaluator
open Parser
open Program

[<TestClass>]
type TestClass () =

    [<TestMethod>]
      member this.TestAddition() =
        let result = parse "2 + 3"
        printfn "%A" result
        let expected = 5
        match result with
        | Some (Plus (Num(a), Num(b))) -> Assert.AreEqual(expected, (a+b))
        | _ -> Assert.Fail("Parsing failed or returned wrong type")
        
        
