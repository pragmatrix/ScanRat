module ScanRat

open ScanRatCombinators
open System.Collections.Generic

// Match a string

let inline (~~) (str:string) = pStr str

// Match one character out of a given string.

let oneOf = pOneOf

// Match the input string starting the given index and return the number of matched characters or None if no match. 
// May pass index = string length, may return Some 0 to match zero characters.

let matchFun (f: string -> int -> int option) = pMatch f

// Match one character

let matchCharFun (f: char -> bool) = 
    let charMatcher (str:string) i = 
        match i with
        | i when i = str.Length -> None
        | i when f str.[i] -> Some 1
        | _ -> None
    matchFun charMatcher --> fun str -> str.[0]

let matchChar c = matchCharFun ((=) c)

type ParsingError = ScanRatMatcher.ParsingError

type ParsingSuccess<'v> = { consumed: int; value: 'v; stats: int list } 
type ParsingFailure = { index: int; expectations: ParsingError list }
    with
        override this.ToString() =
            match this.expectations with
            | hd::tl -> this.index.ToString() + ": " + hd.ToString()
            | _ -> ""

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
        Failure { index = memo.lastErrorPos; expectations = memo.lastError |> Seq.toList }

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
