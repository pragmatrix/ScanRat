module ScanRat.ScanRat

open ScanRat.Combinators

/// Match a string
let inline (~~) (str:string) = pStr str

/// Match one character out of a given string.
let oneOf = pOneOf

/// Match a number of characters before the current location
/// For example !!(matchBack 1 ~~"a") succeeds of no "a" was found at the previous character position 
/// Note that the rule must match all the characters up to the current location to be successful
let matchBack = pMatchBack

/// Match the input string starting the given index and return the number of matched characters or None if no match. 
/// May pass index = string length, may return Some 0 to match zero characters.
let matchFun name (f: string -> int -> int option) = pMatch name f

let matchEnd = pMatch "end" (fun str i -> if i = str.Length then Some 0 else None) --> ignore

/// Match one character
let matchCharFun name (f: char -> bool) = 
    let charMatcher (str:string) i = 
        match i with
        | i when i = str.Length -> None
        | i when f str.[i] -> Some 1
        | _ -> None
    matchFun name charMatcher --> fun str -> str.[0]

let matchChar name c = matchCharFun name ((=) c)

type ParsingError = Matcher.ParsingError

[<NoComparison>]
type ParsingSuccess<'v> = { 
    Consumed: int
    Value: 'v
    Stats: int list
    LastErrorIndex: int
    LastErrorExpectations: ParsingError list 
} 

[<NoComparison>]
type ParsingFailure = { Index: int; Expectations: ParsingError list }
    with
        override this.ToString() =
            match this.Expectations with
            | hd::_tl -> this.Index.ToString() + ": " + hd.ToString()
            | _ -> ""

[<NoComparison>]
type ParsingResult<'v> =
    | Success of ParsingSuccess<'v>
    | Failure of ParsingFailure

let parsePartial parser str =
    let c = ScanRat.Combinators.ParserContext.Create(str)
    let memo = (c :> ScanRat.Matcher.IParseContext).Memo
    match memoParse parser c with
    | ScanRat.Matcher.Success s -> 
        let stats = memo.Stats
        Success { 
            Consumed = s.Next
            Value = s.Value
            Stats = [ stats.Productions; stats.Memo; stats.MemoLR]
            LastErrorIndex = memo.LastErrorPos; LastErrorExpectations = memo.LastError |> Seq.toList 
        }
    | ScanRat.Matcher.Failure f -> 
        Failure { Index = memo.LastErrorPos; Expectations = memo.LastError |> Seq.toList }

let parse parser str = 
    match parsePartial parser str with
    | Success s when s.Consumed <> str.Length ->
        Failure { 
            Index = s.LastErrorIndex
            Expectations = s.LastErrorExpectations 
        }
    | r -> r

let parseq = ParseSequenceBuilder()

let production name = 
    let failingResolver = fun () -> fun _c -> ScanRat.Matcher.Failure { Index = 0 }
    Parser<'a>(name, failingResolver)

let named name p =
    pNamed p name

type Parser<'a> = ScanRat.Combinators.Parser<'a>
