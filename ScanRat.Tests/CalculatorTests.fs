module ScanRat.Tests.Calculator

open NUnit.Framework
open FsUnit

open ScanRat.ScanRat
open ScanRat.Tests.Grammars

[<TestFixture>]
type CalculatorTests() = class
    
    let rec compute exp = 
        match exp with
        | Add (a, b) -> (compute a) + (compute b)
        | Subtract (a, b) -> (compute a) - (compute b)
        | Multiply (a, b) -> (compute a) * (compute b)
        | Divide (a, b) -> (compute a) / (compute b)
        | Number a -> a

    let computeFromResult result = 
        match result with
        | Success s -> compute s.Value
        | Failure _f -> failwith "can't compute, parsing failed"

    let simpleCalc input = 

        let exp = production "exp"
        exp.rule 
            <- exp .+ ~~"+" + exp --> Add
            |- exp .+ ~~"-" + exp --> Subtract
            |- digits --> Number
            |- ~~"(" +. exp .+ ~~")"
            
        parse exp input |> computeFromResult

    // from the IronMeta Project

    let precedenceCalc input = 
        parse precedenceCalcExpression input |> computeFromResult

    [<Test>]
    member _.TestCalcNum() =
        simpleCalc "13" |> should equal 13

    [<Test>]
    member _.TestCalcPlus() =
        simpleCalc "1+2" |> should equal 3

    [<Test>]
    member _.TestCalcMinus() =
        simpleCalc "1-3" |> should equal -2

    [<Test>]
    member _.TestCalcPlusMinus() =
        simpleCalc "1+3-2" |> should equal 2

    [<Test>]
    member _.Braces() =
        simpleCalc "1+(3-2)" |> should equal 2

    [<Test>]
    member _.PrecedenceCalc1() =
        precedenceCalc "1+3*2" |> should equal 7

    [<Test>]
    member _.PrecedenceCalc2() =
        precedenceCalc "3-2*5+3*8+1" |> should equal 18

    [<Test>]
    member _.PrecedenceCalc3() =
        precedenceCalc "3-2/2*5+3*8+1" |> should equal 23

    end
