namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Combinator
open Evaluator
open Parser
open AST
open Program
open System.Collections.Generic

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestNumber() =
        let input = "5"
        let expected = 5
        let result = parse input
        match result with
        | Some (Statements [Num(a)]) -> Assert.AreEqual(expected, a)
        | _ -> Assert.Fail("Parsing failed or returned wrong type")

    [<TestMethod>]
      member this.TestAddition() =
        let result = parse "2 + 3"
        let expected = 5
        match result with
        | Some (Statements [Plus (Num a, Num b)]) -> Assert.AreEqual(expected, (a+b))
        | _ -> Assert.Fail("Parsing failed or returned wrong type")
        

    [<TestMethod>] 
      member this.TestSubtract() =
        let result = parse "5 - 3"
        let expected = 2
        match result with
        | Some (Statements [Minus (Num(a), Num(b))]) -> Assert.AreEqual(expected, (a-b))
        | _ -> Assert.Fail("Parsing failed or returned wrong type")

    [<TestMethod>]   
      member this.TestMultiply() =
        let result = parse "2 * 3"
        let expected = 6
        match result with
        | Some (Statements [Times (Num(a), Num(b))]) -> Assert.AreEqual(expected, (a*b))
        | _ -> Assert.Fail("Parsing failed or returned wrong type")

    [<TestMethod>] 
      member this.TestDivide() =
        let result = parse "6 / 3"
        let expected = 2
        match result with
        | Some (Statements [Divide (Num(a), Num(b))]) -> Assert.AreEqual(expected, (a/b))
        | _ -> Assert.Fail("Parsing failed or returned wrong type")

    [<TestMethod>] 
      member this.TestBoolean() =
        let result = parse "true"
        let expected = true
        match result with
        | Some (Statements [Bbool(b)]) -> Assert.AreEqual(expected, (b))
        | _ -> Assert.Fail("Parsing failed or returned wrong type")
    
    // TEST FOR THE EVALUATOR
    [<TestMethod>] 
      member this.TestIfThen() =
        let example = @"IF true THEN PRINT ""HELLO"""
        let parsedString = parse example
        let expected = "HELLO"
        match parsedString with
        | Some ast -> 
            let env = Dictionary<string, Value>()  // create a new environment
            let evaluatedVal = evaluate env ast
            Assert.AreEqual(expected, evaluatedVal)
        | None -> 
            printfn "Invalid program."
    
        
