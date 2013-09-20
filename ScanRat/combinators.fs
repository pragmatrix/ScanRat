module ScanRatCombinators

open ScanRatMatcher

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
type Parser<'r>(name: string, resolver: unit -> ParseFunc<'r>) = 
    
    let mutable _mutableResolver = resolver;
    let _resolved = lazy (_mutableResolver());

    member this.parse with get() : ParseFunc<'r> = _resolved.Force()
    member this.key with get() = this.parse :> Key
    member this.name with get () = name

    member this.rule with set(p: Parser<'r>) = _mutableResolver <- fun () -> p.parse

let private mkParser name f = Parser(name, fun () -> f)

let private failure index = Failure { ParseFailure.index = index }
let private success index next value = Success { ParseSuccess.index = index; next = next; value = value }
let private success_l index length value = success index (index+length) value

let private refail result = 
    match result with
    | Failure f ->  Failure { ParseFailure.index = f.index }
    | _ -> failwith "can't refail on a success"


let memoParse (parser : Parser<'a>) (c : ParserContext) : ParseResult<'a>= 
    let res = memoCall c parser.name parser.parse
    match res with
    | None -> failure c.index
    | Some item -> downcast item

let pSequence (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a * 'b> =
    fun c -> 
        match memoParse p1 c with
        | Failure _ as f -> refail f
        | Success s1 ->
        match c.at s1.next |> memoParse p2 with
        | Failure f2 -> failure s1.index
        | Success s2 -> 
        let v = (s1.value, s2.value)
        success s1.index s2.next v
    |> mkParser (p1.name + "+" + p2.name)
        
let pChoice (p1 : Parser<'a>) (p2 : Parser<'a>) : Parser<'a> =
    fun c ->
        match memoParse p1 c with
        | Success _ as s -> s
        | r1 ->
        match memoParse p2 c with
        | Success _ as s -> s
        | r2 -> r2
    |> mkParser (p1.name + "|" + p2.name)

let pButNot (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a> =
    fun c ->
        match memoParse p1 c with
        | Failure _ as f -> f
        | Success _ as s ->
        match memoParse p2 c with
        | Success s2 -> failure s2.index
        | Failure _ -> s
    |> mkParser (p1.name + "-" + p2.name)

let pOpt (p : Parser<'a>) : Parser<'a option> =
    fun c ->
        match memoParse p c with
        | Failure f -> success_l f.index 0 None
        | Success s -> success s.index s.next (Some s.value)
    |> mkParser ("[" + p.name + "]") 

let private quote str = "\"" + str + "\""

let pMatch (f : string -> int -> int) =
    fun (c : ParserContext) ->
        let r = f c.text c.index
        if (r <> 0) then
            if c.index + r > c.text.Length then
                failwithf "matched %d characters, but there were only %d left" r (c.text.Length - c.index)
            let matched = c.text.Substring(c.index, r)
            success_l c.index r matched
        else
            failure c.index
    |> mkParser "*?"

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
    |> mkParser (quote str)

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
    |> mkParser ("one of " + quote str)

// Convert a parse result.

let pSelect p f =
    fun c ->
        let r = memoParse p c
        match r with
        | Success s -> success s.index s.next (f s.value)
        | Failure f -> refail r
    |> mkParser p.name

let pNot p = 
    (fun c ->
       let r = memoParse p c
       match r with
       | Success s -> failure s.index
       | Failure f -> success_l f.index 0 ()
    )
    |> mkParser ("!" + p.name)

(* Define some fancy operators!!! *)

type Parser<'resT> 
    with
        static member (-->) (l, f) = pSelect l f 

        static member (+) (l, r) = pSequence l r
        static member (.+) (l, r) = pSequence l r --> fst
        static member (+.) (l, r) = pSequence l r --> snd


        static member (|-) (l, r) = pChoice l r

        static member (/) (l, r) = pButNot l r
        static member (!!) r = pNot r
        static member (+!) (l, r) = l .+ (pNot r)

        member this.opt = pOpt this


(* and ultimately, the Builder *)

type ParseSequenceBuilder() = 
    member this.Bind(p: Parser<'a>, cont : 'a -> Parser<'b>) : Parser<'b> =
        fun c -> 
            match memoParse p c with
            | Failure _ as f -> refail f
            | Success s1 as r1 -> 
            let cnext = c.at s1.next
            let v = s1.value
            let pc = cont v
            let r2 = memoParse pc cnext
            match r2 with
            | Failure f2 -> failure s1.index
            | Success s2 -> 
                let next = if s2.next = 0 then s1.next else s2.next
                success s1.index next s2.value
        |> mkParser p.name

    member this.Return(v) : Parser<'r> = 
        fun c -> success 0 0 v
        |> mkParser ""
        