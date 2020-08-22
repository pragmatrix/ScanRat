module ScanRat.Tests.Performance

open System
open ScanRat.ScanRat
open ScanRat.Tests.Grammars

open FsUnit
open NUnit.Framework

[<TestFixture>]
type PerformanceTests() = 

    let digitGenerator d = (d % 10).ToString().[0]

    let manyDigits = List.init 100000 digitGenerator |> Array.ofList |> (fun ca -> String(ca))

    [<Test>]
    member this.lotsOfDigits() = 
        let r = parse digits manyDigits
        match r with 
        | Failure f -> failwith "failed" 
        | _ -> ()

