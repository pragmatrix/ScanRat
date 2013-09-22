namespace ScanRat.Tests.LogicalPerformanceTests

open NUnit.Framework
open FsUnit

open ScanRat

open TestGrammars

[<TestFixture>]
type LogicalPerformanceTests() = class

    [<Test>]
    member this.number1() =
        let r = parse digits "1"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats |> should equal [10;1;2]

    [<Test>]
    member this.number2() =
        let r = parse digits "42"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats |> should equal [15;1;3]
    
    [<Test>]
    member this.number3() =
        let r = parse digits "911"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats |> should equal [20;1;4]

    [<Test>]
    member this.calculator() =
        let r = parse precedenceCalcExpression "3-2/2*5+3*8+1"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats |> should equal [239;12;42]

    (* direct memoization *)

    [<Test>]
    member this.ambiguitiesMustResultInDirectMemoization() =
        let g =  digits + ~~"Hello"
              |- digits + ~~"World"

        let r = parse g "1World"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats.[1] |> should equal 2

    [<Test>]
    member this.directMemoizationStatDoesNotCaptureLeafs() =
        let g =  digits + ~~"Hello"
              |- digits + ~~"World"

        let r = parse g "42World"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            // only digits are memoized and are returned as a direct result
            s.stats.[1] |> should equal 2

    end
