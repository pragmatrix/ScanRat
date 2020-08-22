/// Tests that clarify assumptions made about the F# language.
module ScanRat.Tests.FSharp

open NUnit.Framework
open FsUnit

open System
open System.Collections.Generic

[<TestFixture>]
type FSharpTests() = class

    [<Test>]
    member _.FunctionsCanBeComparedWhenCastToAnObject() =
        let f = fun _c -> 0
        let fo = f :> obj
        fo |> should equal fo

    [<Test>]
    member _.FunctionsCanBeIdentifiedInADictionaryWithReferenceEqualityWhenCastToObject() =
        let f = fun _c -> 0
        let fo = f :> Object
        let d = new Dictionary<Object, unit>();
        d.Add(fo, ()) |> ignore;
        let r = d.ContainsKey(fo)
        r |> should equal true

end
