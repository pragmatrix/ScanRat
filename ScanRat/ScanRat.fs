module ScanRat

open ScanRatCombinators
open System.Collections.Generic

let inline (~~) (str:string) = pStr str

let oneOf = pOneOf
let matchFun (f: string -> int -> int) = pMatch f
let matchChar (c:char) = matchFun (fun str i -> if str.[i] = c then 1 else 0) --> fun str -> str.[0]

type ParsingError = ScanRatMatcher.ParsingError

type ParsingSuccess<'v> = { consumed: int; value: 'v; stats: int list } 
type ParsingFailure = { index: int; expectations: ParsingError seq }

type ParsingResult<'v> =
    | Success of ParsingSuccess<'v>
    | Failure of ParsingFailure

let parsePartial parser str =
    let c = ScanRatCombinators.ParserContext.create(str)
    let memo = (c :> ScanRatMatcher.IParseContext).memo
    match memoParse parser c with
    | ScanRatMatcher.Success s -> 
        let stats = memo.stats
        Success { consumed = s.next; value = s.value; stats = 
            [ stats.productions; stats.memo; stats.memoLR] }
    | ScanRatMatcher.Failure f -> 
        Failure { index = memo.lastErrorPos; expectations = memo.lastError }

let parse parser str = 
    let r = parsePartial parser str
    match r with
    | Success s when s.consumed <> str.Length -> Failure { index = s.consumed; expectations = [{ expected = "end of input"; stack = []}]}
    | _ -> r

let parseq = ParseSequenceBuilder()

let production name = 
    let failingResolver = fun () -> fun c -> ScanRatMatcher.Failure { index = 0 }
    Parser<'a>(name, failingResolver)

type Parser<'a> = ScanRatCombinators.Parser<'a>
