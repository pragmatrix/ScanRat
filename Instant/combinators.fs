module InstantCombinators

open InstantMatcher

[<Sealed>]
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

type ParseFunc<'r> = ParserContext -> ParseResult<'r>

[<Sealed>]
type Parser<'r>(resolver: unit -> ParseFunc<'r>) = 
    
    let mutable _mutableResolver = resolver;
    let _resolved = lazy (_mutableResolver());

    member this.assign(p: Parser<'r>) : unit =
        _mutableResolver <- fun () -> p.parse

    member this.parse with get() : ParseFunc<'r> = _resolved.Force()
    member this.key with get() = _resolved.Force() :> Key
    member this.rule with set(p: Parser<'r>) = this.assign(p)

let private mkParser f = Parser(fun () -> f)

let private failure index = Failure { ParseFailure.index = index }
let private refail result = 
    match result with
    | Failure f ->  Failure { ParseFailure.index = f.index }
    | _ -> failwith "can't refail on a success"

let private success index next value = Success { ParseSuccess.index = index; next = next; value = value }
let private changeValue result newValue = 
    match result with
    | Failure _ -> failwith "can't changeValue on a parse failure"
    | Success s -> Success { ParseSuccess.index = s.index; next = s.next; value = newValue }


let memoParse (parser : Parser<'a>) (c : ParserContext) : ParseResult<'a>= 
    let res = memoCall c parser.key parser.parse

    match res with
    | None -> failure c.index
    | Some item -> downcast item


let pAnd (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a * 'b> =
    fun c -> 
        match memoParse p1 c with
        | Failure _ as f -> refail f
        | Success s1 ->
        match c.at s1.next |> memoParse p2 with
        // we do fail at the second claus here, this is probably wrong
        | Failure f2 as f -> failure f2.index
        | Success s2 as s -> 
        let v = (s1.value, s2.value)
        success s1.index s2.next v
    |> mkParser
        
let pOr (p1 : Parser<'a>) (p2 : Parser<'a>) : Parser<'a> =
    fun c ->
        match memoParse p1 c with
        | Success _ as s -> s
        | r1 ->
        match memoParse p2 c with
        | Success _ as s -> s
        | r2 -> r2
    |> mkParser

let private success_l index length value = success index (index+length) value

let pStr (str : string) : Parser<string> = 
    fun (c : ParserContext) ->
        if c.index + str.Length > c.text.Length then
            failure c.index
        else
            let extract = c.text.Substring(c.index, str.Length)
            if str = extract then
                success_l c.index str.Length str
            else
                failure c.index
    |> mkParser

let pOneOf (str : string) : Parser<char> =
    fun (c : ParserContext) ->
        if c.index + 1 > c.text.Length then
            failure c.index
        else
            let ch = c.text.[c.index]
            if str.IndexOf(ch) <> -1 then
                success_l c.index 1 ch
            else
                failure c.index
    |> mkParser

// Convert a parse result.

let pSelect p f =
    fun c ->
        let r = memoParse p c
        match r with
        | Success s -> success s.index s.next (f s.value)
        | Failure f -> refail r
    |> mkParser

(* Define some fancy operators!!! *)

type Parser<'resT> 
    with
        static member (-->) (l, f) = pSelect l f 

        static member (+) (l, r) = pAnd l r
        static member (.+) (l, r) = pAnd l r --> fst
        static member (+.) (l, r) = pAnd l r --> snd

        static member (|-) (l, r) = pOr l r

(* and ultimately, the Builder *)

type ParseSequenceBuilder() = 
    member this.Bind(p: Parser<'a>, push : 'a -> Parser<'b>) : Parser<'b> =
        fun c -> 
            match memoParse p c with
            | Failure _ as f -> refail f
            | Success s1 as r1 -> 
            let cnext = c.at s1.next
            let v = s1.value
            let r2 = memoParse (push v) cnext
            // correct "Return" provided results
            match r2 with
            | Failure f2 -> failure s1.index
            | Success s2 -> 
                let next = if s2.next = 0 then s1.next else s2.next
                success s1.index next s2.value
        |> mkParser

    member this.Return(v) : Parser<'r> = 
        fun c -> success 0 0 v
        |> mkParser
        