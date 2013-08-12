namespace Instant.Tests.LogicalPerformanceTests

open NUnit.Framework
open FsUnit

open Instant

open TestGrammars

[<TestFixture>]
type LogicalPerformanceTests() = class

    [<Test>]
    member this.number1() =
        let r = parse digits "1"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats |> should equal [12;0;2]

    [<Test>]
    member this.number2() =
        let r = parse digits "42"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats |> should equal [17;0;3]
    
    [<Test>]
    member this.number3() =
        let r = parse digits "911"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats |> should equal [22;0;4]

    [<Test>]
    member this.calculator() =
        let r = parse precedenceCalcExpression "3-2/2*5+3*8+1"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats |> should equal [353;0;58]

    (* direct memoization *)

    [<Test>]
    member this.ambiguitiesMustResultInDirectMemoization() =
        let g =  digits + ~~"Hello"
              |- digits + ~~"World"

        let r = parse g "1World"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.stats.[1] |> should equal 1

    [<Test>]
    member this.directMemoizationStatDoesNotCaptureLeafs() =
        let g =  digits + ~~"Hello"
              |- digits + ~~"World"

        let r = parse g "42World"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            // only digits is treated as a memoization and the digits are then
            // returned as a direct result
            s.stats.[1] |> should equal 1

    end
