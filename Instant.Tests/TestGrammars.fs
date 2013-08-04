module TestGrammars

open Instant

let digits =
    let digit = oneOf "0123456789" --> fun d -> int(d) - int('0')
    let digits = ref None
    digits := 
        !!digits + digit --> fun (a, b) -> a*10+b
        |= digit
        |> Some
    (!digits).Value

