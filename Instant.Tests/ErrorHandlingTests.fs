namespace Instant.Tests.ErrorHandlingTests

open NUnit.Framework
open FsUnit

open Instant

[<TestFixture>]
type ErrorHandlingTests() = class

    let zeroOrOne = ~~"0" |- ~~"1"

    let simpleGrammar =
        let sg = production();
        sg.rule
            <- sg + zeroOrOne --> fun(a, b) -> a + b
            |- zeroOrOne
        sg

    [<Test>]
    member this.simple1() =
        let r = parse simpleGrammar "012"
        match r with
        | Failure f -> 
            f.index |> should equal 2
        | Success _ -> Assert.Fail()

    end