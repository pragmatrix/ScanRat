module Instant

open InstantCombinators
open System.Collections.Generic

(* shortcuts *)

let inline (~~) (str:string) = pStr str

let inline (!!) (p:Parser<'a> option ref) = pRefer p

let oneOf = pOneOf

let parse parser str = 
    let c = InstantCombinators.ParserContext.create(str)
    memoParse parser c

let parseSeq = ParseSequenceBuilder()
