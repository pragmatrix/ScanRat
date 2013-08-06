module Instant

open InstantCombinators
open System.Collections.Generic

(* shortcuts *)

let inline (~~) (str:string) = pStr str

let oneOf = pOneOf

let parse parser str = 
    let c = InstantCombinators.ParserContext.create(str)
    memoParse parser c

let parser = ParseSequenceBuilder()

let private empty = fun () -> fun c -> ParseResult<'a>(0, 0, None)

let production<'a>() = Parser<'a>(empty)