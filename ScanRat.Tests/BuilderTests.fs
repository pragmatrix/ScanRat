module ScanRat.Tests.Builder

open NUnit.Framework
open FsUnit

open ScanRat.ScanRat

[<TestFixture>]
type BuilderTests() =
    let helloWorldGrammar = parseq {
        let! a = ~~"Hello"
        let! b = ~~"World"
        // Some ... just to change the return type
        return Some (a + "." + b)
    }

    [<Test>]
    member _.SimpleSequence() =
        match parse helloWorldGrammar "HelloWorld"  with
        | Success s ->
            s.Value |> should equal (Some "Hello.World")
            s.Consumed |> should equal 10
        | Failure _f ->
            Assert.Fail()
        
    [<Test>]
    member _.SimpleSequenceFail() =
        match parse helloWorldGrammar "HelloWorl"  with
        | Failure f 
            -> f.Index |> should equal 5
        | _ -> Assert.Fail()
    