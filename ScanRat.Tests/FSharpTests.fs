namespace ScanRat.Tests.FSharpTests

(* Tests that clarify assumptions made about the F# language *)

open NUnit.Framework
open FsUnit

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<TestFixture>]
type FSharpTests() = class

    [<Test>]
    member this.functionsCanNotBeCompared() =
        let f = fun c -> 0
        (f) |> should not' (equal f)

    [<Test>]
    member this.functionsCanBeComparedWhenCastToAnObject() =
        let f = fun c -> 0
        let fo = f :> System.Object
        fo |> should equal fo

    [<Test>]
    member this.functionsCanBeIdentifiedInADictionaryWithReferenceEqualityWhenCastToObject() =
        let f = fun c -> 0
        let fo = f :> Object
        let d = new Dictionary<Object, unit>();
        d.Add(fo, ()) |> ignore;
        let r = d.ContainsKey(fo)
        r |> should equal true

end

