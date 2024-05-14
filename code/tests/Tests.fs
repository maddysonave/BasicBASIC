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
        let expected = 5
        match result with
        | Some (Num (n)) -> Assert.AreEqual(expected, n)
        | _ -> Assert.Fail("Parsing failed or returned wrong type")
        

        //let actual = result + expected

        //Assert.AreEqual(expected, actual)
        
