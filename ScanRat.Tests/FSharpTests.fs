namespace ScanRat.Tests.FSharpTests

(* Tests that clarify assumptions made about the F# language *)

open NUnit.Framework
open FsUnit

open ScanRat

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

end

