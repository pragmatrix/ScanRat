namespace Instant.Tests

open NUnit.Framework
open FsUnit

open Instant
open TestGrammars

[<TestFixture>]
type ParserTests() = class

    let orGrammar = ~~"Hello" |. ~~"World"
    let andGrammar = ~~"Hello" +. ~~"World"


    let helloOrWorldBuilder = 
        ~~"Hello" 
        |. 
        ~~"World" +. ~~"Builder" +* fun (a,b) -> (a + b)

    let lrGrammar =
        let hello = ~~"Hello"

        let g = ref None
        g := !!g +. hello +* fun (a, b) -> (a + b)
             |.
             hello
             |> Some

        (!g).Value

    [<Test>]
    member this.parseOr1() =
        let r = parse orGrammar "Hello"
        r.Value |> should equal "Hello"

    [<Test>]
    member this.parseOr2() =
        let r = parse orGrammar "World"
        r.Value |> should equal "World"

    [<Test>]
    member this.parseOrF() =
        let r = parse orGrammar "W"
        r.value |> should equal None

    [<Test>]
    member this.parseLess() =
        let r = parse orGrammar "HelloWorld"
        r.Value |> should equal "Hello"

    [<Test>]
    member this.ParseAnd2() =
        let r = parse andGrammar "HelloWorld"
        r.Value |> should equal ("Hello", "World")

    [<Test>]
    member this.ParsePrecendence1() =
        let p = parse helloOrWorldBuilder "Hello"
        p.Value |> should equal "Hello"

    [<Test>]
    member this.ParsePrecendence2() =
        let p = parse helloOrWorldBuilder "WorldBuilder"
        p.Value |> should equal "WorldBuilder"

    [<Test>]
    member this.ParsePrecendence3() =
        let p = parse helloOrWorldBuilder "HelloWorldBuilder"
        p.Value |> should equal "Hello"

    [<Test>]
    member this.ParsePrecendence4() =
        let p = parse helloOrWorldBuilder "HelloWorld"
        p.Value |> should equal "Hello"

    [<Test>]
    member this.ParseImmediateLR() =
        let r = parse lrGrammar "HelloHello"
        r.Value |> should equal "HelloHello"

    [<Test>]
    member this.ParseImmediateLR1() =
        let r = parse lrGrammar "Hello"
        r.Value |> should equal "Hello"

    [<Test>]
    member this.ParseImmediateLR3() =
        let r = parse lrGrammar "HelloHelloHello"
        r.Value |> should equal "HelloHelloHello"

    [<Test>]
    member this.ParseDigits1() =
        let r = parse digits "123"
        r.Value |> should equal 123

end
