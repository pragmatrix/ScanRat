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

    end
