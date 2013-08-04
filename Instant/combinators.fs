module InstantCombinators

open InstantMatcher

type ParserContext = { index: int; text: string; memo: Memo }
    with 
        static member create(text: string) : ParserContext =
            { index = 0; text = text; memo = Memo.create() }
        member this.at index =
            { this with index = index };

[<Sealed>]
type ParseResult<'v>(index: int, next: int, value: 'v option) =
    inherit Item(index, next)
    override this.hasValue with get() = value.IsSome

    member this.value : 'v option = value
    member this.failed = this.value.IsNone
    member this.succeeded = not this.failed
    member this.Value = this.value.Value
    member this.fail() = 
        ParseResult(this.index, this.next, None)
    member this.select f = 
        ParseResult(this.index, this.next, if this.value.IsNone then None else Some (f this.Value))
    static member success (index, next, value) =
        ParseResult(index, index, Some value)

type Parser<'resT> = { parse: (ParserContext -> ParseResult<'resT>); id : (unit -> Key) option }

let mkParser f = { parse = f; id = None }
let mkParserWithId f id = { parse = f; id = Some id }

// we must not use with here, because the result may be a different type
let private failParse (p:Item) = ParseResult(p.index, p.next, None)
 

// todo: this should be simplified by parameterizing Item

let memoParse (parser : Parser<'a>) c : ParseResult<'a>= 
    let memo = c.memo
    let production = {
        key = if parser.id.IsSome then parser.id.Value() else (upcast parser); 
        f = fun memo index -> upcast (parser.parse c)
        }
    let res = memoCall memo production c.index

    match res with
    | None -> ParseResult(c.index, c.index, None)
    | Some item -> downcast item


let pAnd (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a * 'b> =
    fun c -> 
        match memoParse p1 c with
        | r1 when r1.failed -> r1.fail()
        | r1 ->
        match c.at r1.next |> memoParse p2 with
        | r2 when r2.failed -> r2.fail()
        | r2 -> 
        ParseResult(r1.index, r2.next, Some (r1.Value, r2.Value))
    |> mkParser

let pOr (p1 : Parser<'a>) (p2 : Parser<'a>) : Parser<'a> =
    fun c ->
        match memoParse p1 c with
        | r1 when r1.succeeded -> r1
        | r1 ->
        match memoParse p2 c with
        | r2 when r2.succeeded -> r2
        | r2 -> failParse r2
    |> mkParser

let private succeed index length value = ParseResult(index, index + length, Some(value))
let private fail index length = ParseResult(index, index + length, None)

let pStr (str : string) : Parser<string> = 
    mkParser (fun c ->
        if c.index + str.Length > c.text.Length then
            fail c.index (c.text.Length - c.index)
        else
        let extract = c.text.Substring(c.index, str.Length)
        if str = extract then
            succeed c.index str.Length str
        else
            fail c.index  (c.index + extract.Length))

let pOneOf (str : string) : Parser<char> =
    mkParser (fun c ->
        if c.index + 1 > c.text.Length then
            fail c.index 1
        else
        let ch = c.text.[c.index]
        if str.IndexOf(ch) <> -1 then
            succeed c.index 1 ch
        else
            fail c.index 1
        )

// Convert a parse result.

let pSelect p f =
    fun c ->
        let r = memoParse p c
        r.select f
    |> mkParser

let pRefer (p : Parser<'a> option ref) =
    mkParserWithId (fun c -> (!p).Value.parse c) (fun () -> upcast !p)
