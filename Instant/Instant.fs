module Instant

open InstantCombinators
open System.Collections.Generic

let inline (~~) (str:string) = pStr str

let oneOf = pOneOf

let parse parser str = 
    let c = InstantCombinators.ParserContext.create(str)
    memoParse parser c

let parser = ParseSequenceBuilder()

let private empty = fun () -> fun c -> InstantMatcher.Failure { index = 0 }

let production<'a>() = Parser<'a>(empty)

type Parser<'a> = InstantCombinators.Parser<'a>

let (|Success|Failure|) result =
    match result with
    | InstantMatcher.Success s -> Success s
    | InstantMatcher.Failure f -> Failure f
