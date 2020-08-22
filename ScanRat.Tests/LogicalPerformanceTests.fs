module ScanRat.Tests.LogicalPerformance

open NUnit.Framework
open FsUnit

open ScanRat.ScanRat
open ScanRat.Tests.Grammars

[<TestFixture>]
type LogicalPerformanceTests() = class

    let leftAndRightRecursiveNestingGrammar = 
        let identifier = (oneOf "abcdefghijklnmopqrstuvwxyz").oneOrMore
        let exp = production("exp")
        let addition = production("addition")        
        
        let primaryExp = production("primaryExp")
        
        exp.rule <- addition

        addition.rule 
            <- addition + ~~"+" + primaryExp --> ignore
            |- primaryExp

        let property = identifier .+ ~~":" + addition
        let objectLiteral = ~~"{" +. property.manySep(~~",") + ~~"}"

        primaryExp.rule
            <- digits --> ignore
            |- objectLiteral --> ignore
            
        exp

    [<Test>]
    member _.Number1() =
        let r = parse digits "1"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.Stats |> should equal [10;1;2]

    [<Test>]
    member _.Number2() =
        let r = parse digits "42"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.Stats |> should equal [15;1;3]
    
    [<Test>]
    member _.Number3() =
        let r = parse digits "911"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.Stats |> should equal [20;1;4]

    [<Test>]
    member __.Calculator() =
        let r = parse precedenceCalcExpression "3-2/2*5+3*8+1"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s -> s.Stats |> should equal [239;12;42]

    // direct memoization

    [<Test>]
    member _.AmbiguitiesMustResultInDirectMemoization() =
        let g =  digits + ~~"Hello"
              |- digits + ~~"World"

        let r = parse g "1World"
        match r with
        | Failure _ -> Assert.Fail()
        | Success s ->
            s.Stats.[1] |> should equal 2

    [<Test>]
    member _.DirectMemoizationStatDoesNotCaptureLeafs() =
        let g =  digits + ~~"Hello"
              |- digits + ~~"World"

        match parse g "42World" with
        | Failure _ -> Assert.Fail()
        | Success s ->
        // only digits are memoized and are returned as a direct result
        s.Stats.[1] |> should equal 2

    [<Test>]
    member _.LeftAndRightRecursiveNestedGrammar() =
        let grammar = leftAndRightRecursiveNestingGrammar
        let r1 = parse grammar "{a:10}"
        let r2 = parse grammar "{a:{b:10}}"
        let r3 = parse grammar "{a:{b:{c:10}}}"

        match r1 with
        | Failure _ -> Assert.Fail()
        | Success s1 -> 
        match r2 with
        | Failure _ -> Assert.Fail()
        | Success s2 -> 
        match r3 with
        | Failure _ -> Assert.Fail()
        | Success s3 -> 
        let d1 = s2.Stats.[0] - s1.Stats.[0]
        let d2 = s3.Stats.[0] - s2.Stats.[0]
        // nesting should only have a linear effect on the production performance
        Assert.AreEqual(d1, d2)

    end
