module Instant

open InstantCombinators

(* shortcuts *)

let inline (~~) (str:string) = pStr str

let inline (!!) (p:Parser<'a> option ref) = pRefer p

let oneOf = pOneOf

let parse parser str = 
    let c = InstantCombinators.ParserContext.create(str)
    memoParse parser c

