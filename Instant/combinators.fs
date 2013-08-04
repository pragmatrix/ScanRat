module InstantCombinators

open InstantMatcher

type ParserContext = { index: int; text: string; memo: Memo }
    with 
        static member create(text: string) : ParserContext =
            { index = 0; text = text; memo = Memo.create() }
        member this.at index =
            { this with index = index };

type ParseResult<'resT> = { index: int; next: int; value: 'resT option }
    with
        member this.failed = this.value.IsNone
        member this.succeeded = not this.failed
        member this.Value = this.value.Value
        member this.fail() = { index = this.index; next = this.next; value = None }
        member this.select f = 
            { index = this.index; next = this.next; value = if this.value.IsNone then None else Some (f this.Value) }
        static member success (index, next, value) =
            { index = index; next = next; value = Some value }

type Parser<'resT> = { parse: (ParserContext -> ParseResult<'resT>); id : (unit -> Key) option }

let mkParser f = { parse = f; id = None }
let mkParserWithId f id = { parse = f; id = Some id }

// we must not use with here, because the result may be a different type
let private failParse p = { index = p.index; next = p.next; value = None }
 

// todo: this should be simplified by parameterizing Item

let memoParse (parser : Parser<'a>) c : ParseResult<'a>= 
    let memo = c.memo
    let production = {
        key = if parser.id.IsSome then parser.id.Value() else (upcast parser); 
        f = fun memo index ->
            let res = parser.parse c
            match res.value with
            | None -> None
            | Some value -> Some { startIndex = res.index; nextIndex = res.next; result = Some (value :> System.Object) }
             }
    let res = memoCall memo production c.index

    match res with
    | None -> { index = c.index; next = c.index; value = None }
    | Some item -> { index = item.startIndex; next = item.nextIndex; value = Some (item.result.Value :?> 'a) }


// no use of with here possible :(
let private select p f = { index = p.index; next = p.next; value = Some (f (p.Value))}

let pAnd (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a * 'b> =
    fun c -> 
        match memoParse p1 c with
        | r1 when r1.failed -> r1.fail()
        | r1 ->
        match c.at r1.next |> memoParse p2 with
        | r2 when r2.failed -> r2.fail()
        | r2 -> 
        { index = r1.index; next = r2.next; value = Some (r1.Value, r2.Value) }
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

let private succeed index length value = { index = index; next = index + length; value = Some value }
let private fail index length = { index = index; next = index + length; value = None }

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
