namespace Instant.Tests

open NUnit.Framework
open FsUnit

open Instant

[<TestFixture>]
type BuilderTests() =
    let helloWorldGrammar = parser {
                let! a = ~~"Hello"
                let! b = ~~"World"
                // Some ... just to change the return type
                return Some (a + "." + b)
            }

    [<Test>]
    member this.simpleSequence() =
        let r = parse helloWorldGrammar "HelloWorld" 
        match r with
        | Success s ->
            s.value |> should equal (Some "Hello.World")
            s.index |> should equal 0
            s.next |> should equal 10
        | Failure f ->
            Assert.Fail()
        
    [<Test>]
    member this.simpleSequenceFail() =
        let r = parse helloWorldGrammar "HelloWorl" 
        match r with
        | Failure f ->
            f.index |> should equal 0
        | _ ->
            Assert.Fail()
        
    