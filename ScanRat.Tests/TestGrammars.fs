module TestGrammars

open ScanRat

let digits =
    let digit = oneOf "0123456789" --> fun d -> int(d) - int('0')

    let digits = production "digits"
    digits.rule 
        <- digits + digit --> fun (a, b) -> a*10+b 
        |- digit

    digits

type Exp = 
    | Add of Exp * Exp
    | Subtract of Exp * Exp
    | Multiply of Exp * Exp
    | Divide of Exp * Exp
    | Number of int

let precedenceCalcExpression = 
    let multiplicative = production "multiplicative"
    let additive = production "additive"
        
    let number = digits --> Number

    let add = additive .+ ~~"+" + multiplicative --> Add
    let sub = additive .+ ~~"-" + multiplicative --> Subtract

    let multiply = multiplicative .+ ~~"*" + number --> Multiply
    let divide = multiplicative .+ ~~"/" + number --> Divide

    additive.rule 
        <- add 
        |- sub 
        |- multiplicative

    multiplicative.rule 
        <- multiply 
        |- divide 
        |- number

    additive
