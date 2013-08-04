namespace Instant.Tests.EdgeCaseTests

open NUnit.Framework
open FsUnit

open Instant

[<TestFixture>]
type EdgeCaseTests() = class

    // Tratt 
    // http://tratt.net/laurie/research/pubs/papers/tratt__direct_left_recursive_parsing_expression_grammars.pdf
    // 5. Incorrect Parses with the Origin LR algorithm from Warth et al.

    
    let tratt51Grammar = 
        let expr = ref None
        let num = oneOf "0123456789"

        expr := !!expr + ~~"-" + !!expr --> fun ((a, _), c) -> "(" + a + "-" + c + ")"
            |= num --> fun c -> string(c)
            |> Some

        (!expr).Value


    // this is actually wrong, in this special case, the production above is handled right-associative
    // according to Tratt, the result should be ((1-2)-3)
    [<Test>]
    member this.tratt51() =
        (parse tratt51Grammar "1-2-3").Value |> should equal "(1-(2-3))"

end
