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
        let a = 2
        let b = 3
        let expected = 5

        let actual = a + b

        Assert.AreEqual(expected, actual)
        
