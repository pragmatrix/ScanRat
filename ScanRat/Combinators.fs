module ScanRatCombinators

open ScanRatMatcher

[<Sealed>]
type ParserContext(index: int, text: string, memo: Memo) = 
    interface IParseContext
        with
            member this.index = index
            member this.memo = memo

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

    member this.parse : ParseFunc<'r> = _resolved.Force()
    member this.name = name

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

// Convert a parse result with index.

let pSelectIndex p f =
    fun c ->
        let r = memoParse p c
        match r with
        | Success s -> success s.index s.next (f (s.index, s.value))
        | Failure f -> refail r
    |> mkParser p.name

let pSelect p f = pSelectIndex p (snd >> f)

// Convert a parse result or fail

let pSelect2 p f =
    fun c ->
        let r = memoParse p c
        match r with
        | Failure f -> refail r
        | Success s -> 
            let fres = f s.value
            match fres with
            | None -> failure c.index
            | Some v -> success s.index s.next v
    |> mkParser p.name

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

let pMany (p : Parser<'a>) : Parser<'a list> =
    let unfolder c =
        match memoParse p c with
        | Failure f -> None
        | Success s -> Some (s, c.at s.next)

    let parseList c = 
        Seq.unfold unfolder c 
    
    fun c ->
        let successes = parseList c
        let a = Seq.toArray successes
        if a.Length = 0 then
            success c.index c.index []
        else
            let lastS = a.[a.Length-1]
            success c.index (lastS.next) (a |> Array.map (fun s -> s.value) |> Array.toList)

    |> mkParser ("[" + p.name + "..]") 

let pMany2 (p : Parser<'a>) (sep: Parser<'b>) : Parser<'a list> =
    fun c ->
        match memoParse p c with
        | Failure f -> success c.index c.index []
        | Success first ->
        let nextc = c.at first.next
        let next = pSelect (pSequence sep p) snd
        match memoParse (pMany next) nextc with
        | Failure f as res -> res
        | Success s -> success c.index s.next (first.value :: s.value)
    |> mkParser ("[" + p.name + sep.name + "..]")

let pMatchBack i (p : Parser<'a>) : Parser<'a> = 
    if (i < 1) then
        failwith "matchBack requires a positive integer as first argument"

    fun (c : ParserContext) ->
        if c.index < i then
            failure c.index
        else
            let cb = c.at (c.index - i)
            // note: we succeed or fail always at the current index, not at the matched one.
            match memoParse p cb with
            | Failure f -> failure c.index
            | Success s -> 
                if s.next <> c.index then
                    failure c.index
                else
                    success_l c.index 0 s.value

    |> mkParser ("@-" + i.ToString() + p.name)

let private quote str = "\"" + str + "\""

let pMatch (name:string) (f : string -> int -> int option) =
    fun (c : ParserContext) ->
        let r = f c.text c.index
        match r with
        | Some r ->
            if c.index + r > c.text.Length then
                failwithf "matched %d characters, but there were only %d left" r (c.text.Length - c.index)
            let matched = c.text.Substring(c.index, r)
            success_l c.index r matched
        | None -> failure c.index
    |> mkParser name

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
    if str = "" then
        failwith "specify one or more characters for pOneOf"
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

let pNot p = 
    fun c ->
        let r = memoParse p c
        match r with
        | Success s -> failure s.index
        | Failure f -> success_l f.index 0 ()
    |> mkParser ("!" + p.name)

let pNamed (p:Parser<'a>) name = 
    fun c -> memoParse p c
    |> mkParser name

(* Define some fancy operators!!! *)

type Parser<'resT> 
    with

        static member (-->) (l, f) = pSelect l f 
        // static member (-->?) (l, f) = pSelect2 l f
        static member (-->.) (l, f) = pSelectIndex l f

        static member (+) (l, r) = pSequence l r
        static member (.+) (l, r) = pSequence l r --> fst
        static member (+.) (l, r) = pSequence l r --> snd

        static member (|-) (l, r) = pChoice l r

        static member (/) (l, r) = pButNot l r
        static member (!!) r = pNot r
        static member (+!) (l, r) = l .+ (pNot r)

        // zero or one (optional)
        member this.opt = pOpt this

        // many (zero or more)
        member this.many = pMany this
        member this.manySep sep = pMany2 this sep
        member this.oneOrMore = pSelect2 (pMany this) (fun l -> if l.Length = 0 then None else Some l)
        member this.oneOrMoreSep sep = pSelect2 (pMany2 this sep) (fun l -> if l.Length = 0 then None else Some l)

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
        