namespace ScanRat.Tests

open NUnit.Framework
open FsUnit

open ScanRat.ScanRat
open ScanRat.Tests.Grammars

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

    let butNotGrammar = 
        (~~"Hello" +! (~~"Hello" + ~~"Builder"));

    let valueOf r =
        match r with
        | Success s -> s.Value
        | Failure _ -> failwith "parse failed"


    [<Test>]
    member _.ParseOr1() =
        let r = parse orGrammar "Hello"
        valueOf r |> should equal "Hello"

    [<Test>]
    member _.ParseOr2() =
        let r = parse orGrammar "World"
        valueOf r |> should equal "World"

    [<Test>]
    member _.ParseOrF() =
        let r = parse orGrammar "W"
        match r with
        | Success _s -> failwith "should not succeed"
        | Failure _f -> ()

    [<Test>]
    member _.ParsePartial() =
        let r = parsePartial orGrammar "HelloWorld"
        valueOf r |> should equal "Hello"

    [<Test>]
    member _.ParseAnd2() =
        let r = parse andGrammar "HelloWorld"
        valueOf r |> should equal ("Hello", "World")

    [<Test>]
    member _.ParsePrecendence1() =
        let r = parse helloOrWorldBuilder "Hello"
        valueOf r |> should equal "Hello"

    [<Test>]
    member _.ParsePrecendence2() =
        let r = parse helloOrWorldBuilder "WorldBuilder"
        valueOf r |> should equal "WorldBuilder"

    [<Test>]
    member _.ParsePrecendence3() =
        let r = parsePartial helloOrWorldBuilder "HelloWorldBuilder"
        valueOf r |> should equal "Hello"

    [<Test>]
    member _.ParsePrecendence4() =
        let r = parsePartial helloOrWorldBuilder "HelloWorld"
        valueOf r |> should equal "Hello"

    [<Test>]
    member _.ParseImmediateLR() =
        let r = parse lrGrammar "HelloHello"
        valueOf r |> should equal "HelloHello"

    [<Test>]
    member _.ParseImmediateLR1() =
        let r = parse lrGrammar "Hello"
        valueOf r |> should equal "Hello"

    [<Test>]
    member _.ParseImmediateLR3() =
        let r = parse lrGrammar "HelloHelloHello"
        valueOf r |> should equal "HelloHelloHello"

    [<Test>]
    member _.ParseDigits1() =
        let r = parse digits "1"
        valueOf r |> should equal 1

    [<Test>]
    member _.ParseDigits2() =
        let r = parse digits "123"
        valueOf r |> should equal 123

    [<Test>]
    member _.PartialButNotShouldNotAdvance() =
        let r = parsePartial butNotGrammar "HelloHello"
        match r with
        | Success { Value = value } ->
            value |> should equal "Hello"
        | Failure _ ->
            failwith "parsing failed"

end
