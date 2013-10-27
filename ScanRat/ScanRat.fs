module ScanRat

open ScanRatCombinators
open System.Collections.Generic

// Match a string

let inline (~~) (str:string) = pStr str

// Match one character out of a given string.

let oneOf = pOneOf

// Match a number of characters before the current location
// For example !!(matchBack 1 ~~"a") succeeds of no "a" was found at the previous character position 
// Note that the rule must match all the characters up to the current location to be successful
let matchBack = pMatchBack

// Match the input string starting the given index and return the number of matched characters or None if no match. 
// May pass index = string length, may return Some 0 to match zero characters.

let matchFun name (f: string -> int -> int option) = pMatch name f

// Match one character

let matchCharFun name (f: char -> bool) = 
    let charMatcher (str:string) i = 
        match i with
        | i when i = str.Length -> None
        | i when f str.[i] -> Some 1
        | _ -> None
    matchFun name charMatcher --> fun str -> str.[0]

let matchChar name c = matchCharFun name ((=) c)

type ParsingError = ScanRatMatcher.ParsingError

type ParsingSuccess<'v> = { consumed: int; value: 'v; stats: int list; lastErrorIndex: int; lastErrorExpectations: ParsingError list } 
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
            [ stats.productions; stats.memo; stats.memoLR];
            lastErrorIndex = memo.lastErrorPos; lastErrorExpectations = memo.lastError |> Seq.toList }
    | ScanRatMatcher.Failure f -> 
        Failure { index = memo.lastErrorPos; expectations = memo.lastError |> Seq.toList }

let parse parser str = 
    let r = parsePartial parser str
    match r with
    | Success s when s.consumed <> str.Length -> Failure { index = s.lastErrorIndex; expectations = s.lastErrorExpectations }
    | _ -> r

let parseq = ParseSequenceBuilder()

let production name = 
    let failingResolver = fun () -> fun c -> ScanRatMatcher.Failure { index = 0 }
    Parser<'a>(name, failingResolver)

type Parser<'a> = ScanRatCombinators.Parser<'a>
