module Instant

open InstantCombinators

(* shortcuts *)

let (|.) = pOr

let (+.) = pAnd
let (~~) = pStr

let (+*) = pSelect

let (!!) = pRefer

let oneOf = pOneOf

let parse parser str = 
    let c = InstantCombinators.ParserContext.create(str)
    memoParse parser c
