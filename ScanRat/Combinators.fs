module ScanRat.Combinators

open ScanRat.Matcher

[<Sealed>]
type ParserContext internal (index: int, text: string, memo: Memo) = 
    interface IParseContext with
        member _.Index = index
        member _.Memo = memo

    member _.Index = index
    member _.Text = text
    member this.At index =
        ParserContext(index, this.Text, (this :> IParseContext).Memo)
    
    static member Create(text: string) : ParserContext =
        ParserContext(0, text, Memo.Create())

type ParseFunc<'r> = ParserContext -> ParseResult<'r>

[<Sealed>]
type Parser<'r>(name: string, resolver: unit -> ParseFunc<'r>) = 
    
    let mutable _mutableResolver = resolver;
    let _resolved = lazy (_mutableResolver());

    member _.Parse : ParseFunc<'r> = _resolved.Force()
    member _.Name = name

    // fsharplint:disable-next-line
    member _.rule with set(p: Parser<'r>) = _mutableResolver <- fun () -> p.Parse

[<AutoOpen>]
module private Private =

    let mkParser name f = Parser(name, fun () -> f)

    let failure index = Failure { ParseFailure.Index = index }
    let success index next value = Success { ParseSuccess.Index = index; Next = next; Value = value }
    let successL index length value = success index (index+length) value

    let refail result = 
        match result with
        | Failure f ->  Failure { ParseFailure.Index = f.Index }
        | _ -> failwith "can't refail on a success"


let memoParse (parser : Parser<'a>) (c : ParserContext) : ParseResult<'a>= 
    let res = memoCall c parser.Name parser.Parse
    match res with
    | None -> failure c.Index
    | Some item -> downcast item

// Convert a parse result with index.

let pSelectIndex p f =
    fun c ->
        let r = memoParse p c
        match r with
        | Success s -> success s.Index s.Next (f (s.Index, s.Value))
        | Failure _f -> refail r
    |> mkParser p.Name

let pSelect p f = pSelectIndex p (snd >> f)

// Convert a parse result or fail

let internal pSelect2 p f =
    fun c ->
        let r = memoParse p c
        match r with
        | Failure _f -> refail r
        | Success s -> 
        let fres = f s.Value
        match fres with
        | None -> failure c.Index
        | Some v -> success s.Index s.Next v
    |> mkParser p.Name

let pSequence (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a * 'b> =
    fun c -> 
        match memoParse p1 c with
        | Failure _ as f -> refail f
        | Success s1 ->
        match c.At s1.Next |> memoParse p2 with
        | Failure f2 -> failure s1.Index
        | Success s2 -> 
        let v = (s1.Value, s2.Value)
        success s1.Index s2.Next v
    |> mkParser (p1.Name + "+" + p2.Name)
        
let pChoice (p1 : Parser<'a>) (p2 : Parser<'a>) : Parser<'a> =
    fun c ->
        match memoParse p1 c with
        | Success _ as s -> s
        | _r1 ->
        match memoParse p2 c with
        | Success _ as s -> s
        | r2 -> r2
    |> mkParser (p1.Name + "|" + p2.Name)

let pButNot (p1 : Parser<'a>) (p2 : Parser<'b>) : Parser<'a> =
    fun c ->
        match memoParse p1 c with
        | Failure _ as f -> f
        | Success _ as s ->
        match memoParse p2 c with
        | Success s2 -> failure s2.Index
        | Failure _ -> s
    |> mkParser (p1.Name + "-" + p2.Name)

let pOpt (p : Parser<'a>) : Parser<'a option> =
    fun c ->
        match memoParse p c with
        | Failure f -> successL f.Index 0 option.None
        | Success s -> success s.Index s.Next (option.Some s.Value)
    |> mkParser ("[" + p.Name + "]") 

let pMany (p : Parser<'a>) : Parser<'a list> =
    let unfolder c =
        match memoParse p c with
        | Failure _f -> option.None
        | Success s -> option.Some (s, c.At s.Next)

    let parseList c = 
        Seq.unfold unfolder c 
    
    fun c ->
        let successes = parseList c
        let a = Seq.toArray successes
        if a.Length = 0 
        then success c.Index c.Index [] else
        let lastS = a.[a.Length-1]
        success c.Index (lastS.Next) (a |> Seq.map (fun s -> s.Value) |> Seq.toList)

    |> mkParser ("[" + p.Name + "..]") 

let pMany2 (p : Parser<'a>) (sep: Parser<'b>) : Parser<'a list> =
    fun c ->
        match memoParse p c with
        | Failure _f -> success c.Index c.Index []
        | Success first ->
        let nextc = c.At first.Next
        let next = pSelect (pSequence sep p) snd
        match memoParse (pMany next) nextc with
        | Failure _f as res -> res
        | Success s -> success c.Index s.Next (first.Value :: s.Value)
    |> mkParser ("[" + p.Name + sep.Name + "..]")

let pMatchBack i (p : Parser<'a>) : Parser<'a> = 
    if (i < 1) then
        failwith "matchBack requires a positive integer as first argument"

    fun (c : ParserContext) ->
        if c.Index < i then failure c.Index else
        let cb = c.At (c.Index - i)
        // note: we succeed or fail always at the current index, not at the matched one.
        match memoParse p cb with
        | Failure _f -> failure c.Index
        | Success s -> 
        if s.Next <> c.Index 
        then failure c.Index
        else successL c.Index 0 s.Value

    |> mkParser ("@-" + i.ToString() + p.Name)

let private quote str = "\"" + str + "\""

let pMatch (name:string) (f : string -> int -> int option) =
    fun (c : ParserContext) ->
        match f c.Text c.Index with
        | option.None -> failure c.Index
        | option.Some r ->
        if c.Index + r > c.Text.Length then
            failwithf "matched %d characters, but there were only %d left" r (c.Text.Length - c.Index)
        let matched = c.Text.Substring(c.Index, r)
        successL c.Index r matched
    |> mkParser name

let pStr (str : string) : Parser<string> = 
    fun (c : ParserContext) ->
        if c.Index + str.Length > c.Text.Length 
        then failure c.Index else
        let extract = c.Text.Substring(c.Index, str.Length)
        if str = extract 
        then successL c.Index str.Length str
        else failure c.Index
    |> mkParser (quote str)

let pOneOf (str : string) : Parser<char> =
    if str = "" then
        failwith "specify one or more characters for pOneOf"
    fun (c : ParserContext) ->
        if c.Index + 1 > c.Text.Length 
        then failure c.Index else
        let ch = c.Text.[c.Index]
        if str.IndexOf(ch) <> -1 
        then successL c.Index 1 ch
        else failure c.Index
    |> mkParser ("one of " + quote str)

let pNot p = 
    fun c ->
        match memoParse p c with
        | Success s -> failure s.Index
        | Failure f -> successL f.Index 0 ()
    |> mkParser ("!" + p.Name)

let pNamed (p:Parser<'a>) name = 
    fun c -> memoParse p c
    |> mkParser name

(* Define some fancy operators!!! *)

type Parser<'resT> with
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

    // fsharplint:disable

    // zero or one (optional)
    member this.opt = pOpt this

    // many (zero or more)
    member this.many = pMany this
    member this.manySep sep = pMany2 this sep
    member this.oneOrMore = pSelect2 (pMany this) (fun l -> if l.Length = 0 then None else Some l)
    member this.oneOrMoreSep sep = pSelect2 (pMany2 this sep) (fun l -> if l.Length = 0 then None else Some l)

    // fsharplint:enable

(* and ultimately, the Builder *)

type ParseSequenceBuilder() = 
    member _.Bind(p: Parser<'a>, cont : 'a -> Parser<'b>) : Parser<'b> =
        fun c -> 
            match memoParse p c with
            | Failure _ as f -> refail f
            | Success s1 -> 
            let cnext = c.At s1.Next
            let v = s1.Value
            let pc = cont v
            let r2 = memoParse pc cnext
            match r2 with
            | Failure _f2 -> failure s1.Index
            | Success s2 -> 
                let next = if s2.Next = 0 then s1.Next else s2.Next
                success s1.Index next s2.Value
        |> mkParser p.Name

    member _.Return(v) : Parser<'r> = 
        fun _c -> success 0 0 v
        |> mkParser ""
        