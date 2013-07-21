namespace Instant.Tests

open InstantCombinators

open NUnit.Framework
open FsUnit

open Instant


[<TestFixture>]
type ParserTests() = class
    [<Test>]
    member this.parseSimple() =

        let grammar = ~~"Hello" |. ~~"World"
        let f = parse grammar "Hello"
        let f1 = parse grammar "Helli"
        let f2 = parse grammar "World"
        ()
    end
