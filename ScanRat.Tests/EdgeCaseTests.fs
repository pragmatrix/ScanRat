module ScanRat.Tests.EdgeCase

open NUnit.Framework
open FsUnit

open ScanRat.ScanRat

[<TestFixture>]
type EdgeCaseTests() = class

    // Tratt 
    // http://tratt.net/laurie/research/pubs/papers/tratt__direct_left_recursive_parsing_expression_grammars.pdf
    // 5. Incorrect Parses with the Origin LR algorithm from Warth et al.

    
    let tratt51Grammar = 
        let num = oneOf "0123456789"

        let expr = production "expr"
        expr.rule 
            <- expr + ~~"-" + expr --> fun ((a, _), c) -> "(" + a + "-" + c + ")"
            |- num --> fun c -> string(c)
            
        expr


    // this is actually wrong, in this special case, the production above is handled right-associative
    // according to Tratt, the result should be ((1-2)-3)
    [<Test>]
    member _.Tratt51() =
        match parse tratt51Grammar "1-2-3" with
        | Success s -> s.Value |> should equal "(1-(2-3))"
        | Failure f -> failwith "failed"

end
