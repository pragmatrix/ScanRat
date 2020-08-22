module ScanRat.Tests.ErrorHandling

open NUnit.Framework
open FsUnit

open ScanRat.ScanRat

[<TestFixture>]
type ErrorHandlingTests() = class

    let zeroOrOne = ~~"0" |- ~~"1"

    let simpleGrammar =
        let sg = production "sg";
        sg.rule
            <- sg + zeroOrOne --> fun (a, b) -> a + b
            |- zeroOrOne
        sg


    let oneAndTwo = ~~"one" + ~~"two" --> fun (a, b) -> a + b

    let oneTwoOrOneOne = oneAndTwo |- ~~"one" + ~~"one"  --> fun (a, b) -> a + b

    [<Test>]
    member _.ExpectedEndOfInput() =
        match parse simpleGrammar "012" with
        | Failure f -> 
            f.Index |> should equal 2
        | Success _ -> Assert.Fail()

    [<Test>]
    member _.ExpectedSomethingElse() =
        match parse oneAndTwo "onetwe" with
        | Failure f -> 
            f.Index |> should equal 3
            f.Expectations |> Seq.map (fun r -> r.Expected) |> should equal ["\"two\""]
        | Success _ -> Assert.Fail()

    [<Test>]
    member _.ExpectedTwoOtherStrings() =
        match parse oneTwoOrOneOne "onetwe" with
        | Failure f -> 
            f.Index |> should equal 3
            f.Expectations |> Seq.map (fun r -> r.Expected) |> should equal ["\"two\""; "\"one\""]
        | Success _ -> Assert.Fail()

    [<Test>]
    member _.DontReportParentClausesAtTheSameIndex() =
        let p = ~~"one" |- ~~"two"
        match parse  p "x" with
        | Failure f ->
            f.Index |> should equal 0
            f.Expectations |> Seq.length |> should equal 2
        | Success _ -> Assert.Fail()
    end