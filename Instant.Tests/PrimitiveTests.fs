namespace Instant.Tests

open NUnit.Framework
open FsUnit

open Instant
open TestGrammars


[<TestFixture>]
type PrimitiveTests() = class

    let orGrammar = ~~"Hello" |- ~~"World"
    let andGrammar = ~~"Hello" + ~~"World"


    let helloOrWorldBuilder
        =  ~~"Hello" 
        |- ~~"World" + ~~"Builder" --> fun (a, b) -> (a + b)

    let lrGrammar =
        let hello = ~~"Hello"

        let g = production "g"
        g.rule 
            <- g + hello --> fun (a, b) -> (a + b)
            |- hello
        g

    let valueOf r =
        match r with
        | Success s -> s.value
        | Failure _ -> failwith "parse failed"

    [<Test>]
    member this.parseOr1() =
        let r = parse orGrammar "Hello"
        valueOf r |> should equal "Hello"

    [<Test>]
    member this.parseOr2() =
        let r = parse orGrammar "World"
        valueOf r |> should equal "World"

    [<Test>]
    member this.parseOrF() =
        let r = parse orGrammar "W"
        match r with
        | Success s -> failwith "should not succeed"
        | Failure f -> ()

    [<Test>]
    member this.parsePartial() =
        let r = parsePartial orGrammar "HelloWorld"
        valueOf r |> should equal "Hello"

    [<Test>]
    member this.ParseAnd2() =
        let r = parse andGrammar "HelloWorld"
        valueOf r |> should equal ("Hello", "World")

    [<Test>]
    member this.ParsePrecendence1() =
        let r = parse helloOrWorldBuilder "Hello"
        valueOf r |> should equal "Hello"

    [<Test>]
    member this.ParsePrecendence2() =
        let r = parse helloOrWorldBuilder "WorldBuilder"
        valueOf r |> should equal "WorldBuilder"

    [<Test>]
    member this.ParsePrecendence3() =
        let r = parsePartial helloOrWorldBuilder "HelloWorldBuilder"
        valueOf r |> should equal "Hello"

    [<Test>]
    member this.ParsePrecendence4() =
        let r = parsePartial helloOrWorldBuilder "HelloWorld"
        valueOf r |> should equal "Hello"

    [<Test>]
    member this.ParseImmediateLR() =
        let r = parse lrGrammar "HelloHello"
        valueOf r |> should equal "HelloHello"

    [<Test>]
    member this.ParseImmediateLR1() =
        let r = parse lrGrammar "Hello"
        valueOf r |> should equal "Hello"

    [<Test>]
    member this.ParseImmediateLR3() =
        let r = parse lrGrammar "HelloHelloHello"
        valueOf r |> should equal "HelloHelloHello"

    [<Test>]
    member this.ParseDigits1() =
        let r = parse digits "123"
        valueOf r |> should equal 123

end
