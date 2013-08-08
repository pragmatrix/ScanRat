namespace Instant.Tests

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

    let computeFromResult result = 
        match result with
        | Success s -> compute s.value
        | Failure f -> failwith "can't compute, parsing failed"

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
        let multiplicative = production "multiplicative"
        let additive = production "additive"
        
        let number = digits --> Number

        let add = additive .+ ~~"+" + multiplicative --> Add
        let sub = additive .+ ~~"-" + multiplicative --> Subtract

        let multiply = multiplicative .+ ~~"*" + number --> Multiply
        let divide = multiplicative .+ ~~"/" + number --> Divide

        additive.rule 
            <- add 
            |- sub 
            |- multiplicative

        multiplicative.rule 
            <- multiply 
            |- divide 
            |- number

        let expression = additive

        parse expression input |> computeFromResult

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
