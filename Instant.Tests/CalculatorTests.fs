namespace Instant.Tests.CalculatorTests

open NUnit.Framework
open FsUnit

open Instant
open TestGrammars

type Exp = 
    | Plus of Exp * Exp
    | Minus of Exp * Exp
    | Number of int

[<TestFixture>]
type ParserTests() = class
    
    let simpleCalc input = 

        let exp = ref None
        exp := !!exp +. ~~"+" +. !!exp +* fun ((a, _), c) -> (Plus (a, c))
            |. !!exp +. ~~"-" +. !!exp +* fun ((a, _), c) -> (Minus (a, c))
            |. digits +* fun n -> Number n
            |. ~~"(" +. !!exp +. ~~")" +* fun ((_, e), _) -> e
            |> Some

        let rec compute exp = 
            match exp with
            | Plus (a, b) -> (compute a) + (compute b)
            | Minus (a, b) -> (compute a) - (compute b)
            | Number a -> a

        let grammar = (!exp).Value
        (parse grammar input).Value |> compute

    [<Test>]
    member this.testCalcNum() =
        simpleCalc "13" |> should equal 13

    [<Test>]
    member this.testCalcPlus() =
        simpleCalc "1+2" |> should equal 3

    [<Test>]
    member this.testCalcMinus() =
        simpleCalc "1-3" |> should equal -2

    [<Test>]
    member this.testCalcPlusMinus() =
        simpleCalc "1+3-2" |> should equal 2

    [<Test>]
    member this.braces() =
        simpleCalc "1+(3-2)" |> should equal 2

    end
