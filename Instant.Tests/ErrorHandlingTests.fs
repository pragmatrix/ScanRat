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
            <- sg + zeroOrOne --> fun (a, b) -> a + b
            |- zeroOrOne
        sg


    let oneAndTwo = ~~"one" + ~~"two" --> fun (a, b) -> a + b

    let oneTwoOrOneOne = oneAndTwo |- ~~"oneone"

    [<Test>]
    member this.expectedEndOfInput() =
        let r = parse simpleGrammar "012"
        match r with
        | Failure f -> 
            f.index |> should equal 2
        | Success _ -> Assert.Fail()



    [<Test>]
    member this.expectedSomethingElse() =
        let r = parse oneAndTwo "onetwe"
        match r with
        | Failure f -> 
            f.index |> should equal 3
        | Success _ -> Assert.Fail()


    [<Test>]
    member this.errorRecordButNoError() =
        let r = parse oneTwoOrOneOne "oneone"
        match r with
        | Failure f -> 
            f.index |> should equal 3
        | Success _ -> Assert.Fail()

    end