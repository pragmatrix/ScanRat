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
        static member success (index, next, value) =
            { index = index; next = next; value = Some value }

type Parser<'resT> = { parse: (ParserContext -> ParseResult<'resT>) }

// we must not use with here, because the result may be a different type
let private failParse p = { index = p.index; next = p.next; value = None }
 

let parse p = p.parse



// here, too :(
let private select p f = { index = p.index; next = p.next; value = Some (f (p.Value))}

let pAnd (p1 : Parser<'a>) (p2 : Parser<'b>) =
    {
        parse = fun c -> 
            match parse p1 c with
            | r1 when r1.failed -> r1.fail()
            | r1 ->
            match c.at r1.next |> parse p2 with
            | r2 when r2.failed -> r2.fail()
            | r2 -> 
            { index = r1.index; next = r2.next; value = Some (r1.Value, r2.Value) }
    }

type Either<'a, 'b> = 
    | First of 'a 
    | Second of 'b

let pOr (p1 : Parser<'a>) (p2 : Parser<'b>) =
    {
        parse = fun c ->
            match parse p1 c with
            | r1 when r1.succeeded -> select r1 First
            | r1 ->
            match parse p2 c with
            | r2 when r2.succeeded -> select r2 Second
            | r2 -> failParse r2
    }

let private succeed index length value = { index = index; next = index + length; value = Some value }
let private fail index length = { index = index; next = index + length; value = None }

let pStr (str : string) : Parser<string> = 
    {
        parse = fun c ->
            let extract = c.text.Substring(c.index, str.Length)
            if str = extract then
                succeed c.index str.Length str
            else
                fail c.index  (c.index + extract.Length)
    }

