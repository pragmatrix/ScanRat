module ScanRat.Tests.Performance

open System
open ScanRat.ScanRat
open ScanRat.Tests.Grammars

open NUnit.Framework

[<TestFixture>]
type PerformanceTests() = 

    let digitGenerator d = (d % 10).ToString().[0]

    let manyDigits = List.init 100000 digitGenerator |> Array.ofList |> (fun ca -> String(ca))

    [<Test>]
    member _.LotsOfDigits() = 
        match parse digits manyDigits with 
        | Failure _f -> failwith "failed" 
        | _ -> ()

