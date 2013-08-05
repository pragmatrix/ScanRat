namespace Instant.Tests

open NUnit.Framework
open FsUnit

open Instant

[<TestFixture>]
type BuilderTests() =
    let helloWorldGrammar = 
        parseSeq {
                let! a = ~~"Hello"
                let! b = ~~"World"
                // Some ... just to change the return type
                return Some (a + "." + b)
            }

    [<Test>]
    member this.simpleSequence() =
        let r = parse helloWorldGrammar "HelloWorld" 
        
        r.Value |> should equal (Some "Hello.World")
        r.index |> should equal 0
        r.next |> should equal 10
        
    [<Test>]
    member this.simpleSequenceFail() =
        let r = parse helloWorldGrammar "HelloWorl" 

        r.failed |> should equal true
        r.index |> should equal 0
        r.next |> should equal 0

        
    