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
    member this.TestNumber() =
        let input = "5"
        let expected = 5
        let result = parse input
        match result with
        | Some (Statements [Num(a)]) -> Assert.AreEqual(expected, a)
        | _ -> Assert.Fail("Parsing failed or returned wrong type")

    [<TestMethod>]
    member this.TestString() =
        let input = "Hello"
        let expected = "Hello"
        let result = parse input
        match result with
        | Some (Statements [Bstring(a)]) -> Assert.AreEqual(expected,  a)
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
    member this.TestPrint() =
        let input = "PRINT " 
        let expected = " "
        let result = parse input
        match result with
        | Some (Statements [Print (Bstring a)]) -> Assert.AreEqual(expected, a)
        | _ -> Assert.Fail("Parsing failed or returned wrong type")

    
        
