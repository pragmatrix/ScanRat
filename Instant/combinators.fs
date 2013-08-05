module InstantCombinators

open InstantMatcher

type ParserContext(index: int, text: string, memo: Memo) = 
    interface IParseContext
        with
            member this.index with get() = index
            member this.memo with get() = memo

    member this.index = index
    member this.text = text
    member this.at index =
        ParserContext(index, this.text, (this :> IParseContext).memo)
    
    static member create(text: string) : ParserContext =
        ParserContext(0, text, Memo.create())

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
    with
        member this.key with get() = if this.id.IsNone then this :> Key else this.id.Value()

let mkParser f = { parse = f; id = None }
let mkParserWithId f id = { parse = f; id = Some id }

// we must not use with here, because the result may be a different type
let private failParse (p:Item) = ParseResult(p.index, p.next, None)
 

let memoParse (parser : Parser<'a>) (c : ParserContext) : ParseResult<'a>= 
    let res = memoCall c parser.key parser.parse

    match res with
    | None -> ParseResult(c.index, c.index, None)
    | Some item -> downcast item


let pAnd (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a * 'b> =
    fun c -> 
        match memoParse p1 c with
        | r1 when r1.failed -> r1.fail()
        | r1 ->
        match c.at r1.next |> memoParse p2 with
        // we do completely fail here, this is probably wrong
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
    fun (c : ParserContext) ->
        if c.index + str.Length > c.text.Length then
            fail c.index (c.text.Length - c.index)
        else
            let extract = c.text.Substring(c.index, str.Length)
            if str = extract then
                succeed c.index str.Length str
            else
                fail c.index  (c.index + extract.Length)
    |> mkParser

let pOneOf (str : string) : Parser<char> =
    fun (c : ParserContext) ->
        if c.index + 1 > c.text.Length then
            fail c.index 1
        else
            let ch = c.text.[c.index]
            if str.IndexOf(ch) <> -1 then
                succeed c.index 1 ch
            else
                fail c.index 1
    |> mkParser

// Convert a parse result.

let pSelect p f =
    fun c ->
        let r = memoParse p c
        r.select f
    |> mkParser

let pRefer (p : Parser<'a> option ref) =
    mkParserWithId (fun c -> (!p).Value.parse c) (fun () -> upcast !p)


(* Define some fancy operators!!! *)

type Parser<'resT> 
    with
        static member (|=) (l, r) = pOr l r
        static member (-->) (l, f) = pSelect l f 

        static member (+) (l, r) = pAnd l r
        static member (.+) (l, r) = pAnd l r --> fun (a, b) -> a
        static member (+.) (l, r) = pAnd l r --> fun (a, b) -> b

(* and ultimately, the Builder *)

type ParseSequenceBuilder() = 
    member this.Bind(p: Parser<'a>, push : 'a -> Parser<'b>) : Parser<'b> =
        fun c -> 
            match memoParse p c with
            | r1 when r1.failed -> r1.fail()
            | r1 -> 
            let cnext = c.at r1.next
            let v = r1.Value
            let r2 = memoParse (push v) cnext
            // correct "Return" provided results
            let next = if r2.next = 0 then r1.next else r2.next
            let length = next - r1.index
            match r2 with
            | r2 when r2.failed -> fail r1.index length
            | r2 -> succeed r1.index length r2.Value
        |> mkParser

    member this.Return(v) : Parser<'r> = 
        fun c -> succeed 0 0 v
        |> mkParser
        