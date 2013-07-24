namespace Instant.Tests

open InstantCombinators

open NUnit.Framework
open FsUnit

open Instant


[<TestFixture>]
type ParserTests() = class

    let orGrammar = ~~"Hello" |. ~~"World"
    let andGrammar = ~~"Hello" &. ~~"World"

    let lrGrammar =
        let g = ref None 
        let hello = ~~"Hello"
        g := Some(
                hello
                |.
                !!g &. hello >* fun (a, b) -> (a + b))
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
    member this.ParseImmediateLR() =
        let r = parse lrGrammar "HelloHello"
        r.Value |> should equal "HelloHello"



end
