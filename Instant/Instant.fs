module Instant

(* shortcuts *)

let (|.) = InstantCombinators.pOr
let (&.) = InstantCombinators.pAnd
let (~~) = InstantCombinators.pStr

let parse parser str = 
    let c = InstantCombinators.ParserContext.create(str)
    InstantCombinators.parse parser c
