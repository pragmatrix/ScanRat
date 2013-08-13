module Instant

open InstantCombinators
open System.Collections.Generic

let inline (~~) (str:string) = pStr str

let oneOf = pOneOf

type ParsingError = InstantMatcher.ErrorRecord

type ParsingSuccess<'v> = { consumed: int; value: 'v; stats: int list } 
type ParsingFailure = { index: int; expectations: ParsingError seq }

type ParsingResult<'v> =
    | Success of ParsingSuccess<'v>
    | Failure of ParsingFailure

let parsePartial parser str =
    let c = InstantCombinators.ParserContext.create(str)
    let memo = (c :> InstantMatcher.IParseContext).memo
    match memoParse parser c with
    | InstantMatcher.Success s -> 
        let stats = memo.stats
        Success { consumed = s.next; value = s.value; stats = 
            [ stats.productions; stats.memo; stats.memoLR] }
    | InstantMatcher.Failure f -> 
        Failure { index = memo.lastErrorPos; expectations = memo.lastErrorRecords }

let parse parser str = 
    let r = parsePartial parser str
    match r with
    | Success s when s.consumed <> str.Length -> Failure { index = s.consumed; expectations = [{ expected = "end of input"; callStack = []}]}
    | _ -> r

let parser = ParseSequenceBuilder()

let production name = 
    let failingResolver = fun () -> fun c -> InstantMatcher.Failure { index = 0 }
    Parser<'a>(name, failingResolver)

type Parser<'a> = InstantCombinators.Parser<'a>
