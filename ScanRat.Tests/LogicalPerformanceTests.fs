namespace ScanRat.Tests.LogicalPerformanceTests

open NUnit.Framework
open FsUnit

open ScanRat

open TestGrammars

[<TestFixture>]
type LogicalPerformanceTests() = class

    let leftAndRightRecursiveNestingGrammar = 
        let identifier = (oneOf "abcdefghijklnmopqrstuvwxyz").oneOrMore
        let exp = production("exp")
        let addition = production("addition")        
        
        let primaryExp = production("primaryExp")
        
        exp.rule <- addition

        addition.rule 
            <- addition + ~~"+" + primaryExp --> fun _ -> ()
            |- primaryExp

        let property = identifier .+ ~~":" + addition
        let objectLiteral = ~~"{" +. property.manySep(~~",") + ~~"}"

        primaryExp.rule
            <- digits --> fun _ -> ()
            |- objectLiteral --> fun _ -> ()
            
        exp

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

    [<Test>]
    member this.leftAndRightRecursiveNestedGrammar() =
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
                    let d1 = s2.stats.[0] - s1.stats.[0]
                    let d2 = s3.stats.[0] - s2.stats.[0]
                    // nesting should only have a linear effect on the production performance
                    Assert.AreEqual(d1, d2)

    end
