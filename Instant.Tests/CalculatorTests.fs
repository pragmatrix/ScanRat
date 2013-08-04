namespace Instant.Tests.CalculatorTests

open NUnit.Framework
open FsUnit

open Instant
open TestGrammars

type Exp = 
    | Add of Exp * Exp
    | Subtract of Exp * Exp
    | Multiply of Exp * Exp
    | Divide of Exp * Exp
    | Number of int

[<TestFixture>]
type ParserTests() = class
    
    let rec compute exp = 
        match exp with
        | Add (a, b) -> (compute a) + (compute b)
        | Subtract (a, b) -> (compute a) - (compute b)
        | Multiply (a, b) -> (compute a) * (compute b)
        | Divide (a, b) -> (compute a) / (compute b)
        | Number a -> a

    let simpleCalc input = 

        let exp = ref None
        exp := !!exp .+ ~~"+" + !!exp --> fun (a, b) -> Add (a, b)
            |= !!exp .+ ~~"-" + !!exp --> fun (a, b) -> Subtract (a, b)
            |= digits --> fun n -> Number n
            |= ~~"(" + !!exp + ~~")" --> fun ((_, e), _) -> e
            |> Some

        let grammar = (!exp).Value
        (parse grammar input).Value |> compute

    // from the IronMeta Project

    let precedenceCalc input = 
        let expression = ref None
        let multiplicative = ref None
        let additive = ref None
        
        let number = digits --> fun d -> Number d

        let add = !!additive + ~~"+" + !!multiplicative --> fun ((a, _), c) -> Add(a, c)
        let sub = !!additive + ~~"-" + !!multiplicative --> fun ((a, _), c) -> Subtract(a, c)

        let multiply = !!multiplicative + ~~"*" + number --> fun ((a, _), c) -> Multiply(a, c)
        let divide = !!multiplicative + ~~"/" + number --> fun ((a, _), c) -> Divide(a, c)


        additive := Some (add |= sub |= !!multiplicative)
        multiplicative := Some (multiply |= divide |= number)
        expression := Some (!additive).Value

        let grammar = (!expression).Value
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

    [<Test>]
    member this.precedenceCalc1() =
        precedenceCalc "1+3*2" |> should equal 7

    [<Test>]
    member this.precedenceCalc2() =
        precedenceCalc "3-2*5+3*8+1" |> should equal 18

    [<Test>]
    member this.precedenceCalc3() =
        precedenceCalc "3-2/2*5+3*8+1" |> should equal 23

    end
