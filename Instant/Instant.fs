module Instant

open InstantCombinators
open System.Collections.Generic

let inline (~~) (str:string) = pStr str

let oneOf = pOneOf


let (|Success|Failure|) result =
    match result with
    | InstantMatcher.Success s -> Success s
    | InstantMatcher.Failure f -> Failure f

let parsePartial parser str =
    let c = InstantCombinators.ParserContext.create(str)
    memoParse parser c

let parse parser str = 
    let r = parsePartial parser str
    match r with
    | Success s ->
        if s.next <> str.Length then InstantMatcher.Failure { index = s.next } else r
    | Failure _ -> r

let parser = ParseSequenceBuilder()

let production name = 
    let failingResolver = fun () -> fun c -> InstantMatcher.Failure { index = 0 }
    Parser<'a>(name, failingResolver)

type Parser<'a> = InstantCombinators.Parser<'a>
