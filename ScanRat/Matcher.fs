module ScanRat.Matcher

open System
open System.Collections.Generic
open System.Linq

type Dictionary<'k, 'v> with
    member inline this.TryFind key =
        match this.TryGetValue key with
        | (true, v) -> Some v
        | (false, _) -> None

type ParseSuccess<'v> = { 
    Value: 'v 
    Index: int 
    Next: int 
}

type ParseFailure = { Index: int }

type IItem =
    abstract member Next : int
    abstract member IsSuccess : bool

type ParseResult<'v> =
    | Success of ParseSuccess<'v>
    | Failure of ParseFailure

    interface IItem with
        member this.Next =
            match this with
            | Success ps -> ps.Next
            | _ -> failwith "no next"
        member this.IsSuccess =
            match this with
            | Success _ -> true
            | _ -> false
   
type Key = obj

type Error = { 
    Message: string
    Index: int
}

type RuleTable = Dictionary<int, IItem option>
type ExpansionTable = Dictionary<int, RuleTable>

type MemoTable = Dictionary<Key, ExpansionTable>

[<NoComparison>]
type Expansion = {
    Key: Key
    Num : int 
}

[<NoComparison>]
type LRRecord = {
    mutable Expansion: Expansion
    mutable LRDetected: bool
    mutable NextIndex: int
    mutable Result: IItem option
    Involved: HashSet<Key>
    Name : string
}

type RecordTable = Dictionary<int, LRRecord>
type LRTable = Dictionary<Key, RecordTable>

type Stats = {
    // Number of actual production calls
    mutable Productions: int
    // Number of successful memo lookups
    mutable Memo: int
    // Number of successful lr memo lookupos
    mutable MemoLR: int
} with 
    member this.TrackProduction() = this.Productions <- this.Productions + 1
    member this.TrackMemo() = this.Memo <- this.Memo + 1
    member this.TrackMemoLR() = this.MemoLR <- this.MemoLR + 1

[<NoComparison>]
type ErrorRecord = {
    Key: Object
    Expected: string
    CallStack: LRRecord list
}

[<NoComparison>]
type ParsingError = { 
    Expected: string
    Stack: string seq 
} with 
    override this.ToString() =
        this.Expected

[<NoComparison>]
type Memo = {
    Table: MemoTable
    Recursions: LRTable
    mutable CallStack: LRRecord list
    LastErrorRecords: List<ErrorRecord>
    mutable LastErrorPos: int
    Stats: Stats
} with 
    static member Create() = {
        Table = new MemoTable()
        Recursions = new LRTable()
        CallStack = List.empty
        LastErrorRecords = new List<_>()
        LastErrorPos = -1
        Stats = { 
            Memo = 0
            MemoLR = 0
            Productions = 0 
        }
    }
    member this.LastError : ParsingError seq =
        let keys = new HashSet<_>()
        seq {
            for er in this.LastErrorRecords do
                if not (keys.Contains er.Key) then
                    yield { ParsingError.Expected = er.Expected; Stack = List.map (fun lr -> lr.Name) er.CallStack }
                    List.iter (fun lr -> keys.Add(lr.Expansion.Key) |> ignore) er.CallStack
                    keys.Add(er.Key) |> ignore
        }

type IParseContext =
    abstract member Memo : Memo
    abstract member Index : int

exception MatcherException of Error

let rec memoCall (context: 'c :> IParseContext) (name: string) (production : 'c -> 'r :> IItem) : (IItem option) =
    let memo = context.Memo
    let index = context.Index
    let key = production :> Key
    let expansion = { Key = key; Num = 0 }

    match tryGetMemo memo expansion index with
    | Some result -> 
        memo.Stats.TrackMemo()
        result
    | _ ->

    match tryGetLRRecord memo expansion index with
    | Some record ->
        record.LRDetected <- true

        let involved = memo.CallStack.TakeWhile(fun lr -> lr.Expansion.Key <> expansion.Key).Select(fun lr -> lr.Expansion.Key)
        record.Involved.UnionWith involved
        
        match tryGetMemo memo record.Expansion index with
        | None -> raise (MatcherException({ Index = index; Message = "Problem with expansion" }))
        | Some result -> 
            memo.Stats.TrackMemoLR()
            result
    | None ->

    // no lr information

    let recordExpansion = { expansion with Num = 1 }
    memoize memo recordExpansion index None

    let record = { 
        LRRecord.LRDetected = false
        Expansion = recordExpansion
        NextIndex = -1
        Result = None
        Name = name
        Involved = new HashSet<Key>() 
    }

    startLRRecord memo expansion index record
    memo.CallStack <- record :: memo.CallStack

    let rec resolveItem() : IItem option = 

        // printf "%d %d %s\n" context.index record.expansion.num name
        let pResult = production context :> IItem
        memo.Stats.TrackProduction()

        let result = if pResult.IsSuccess then (Some pResult) else None
        // do we need to keep trying the expansions?
        
        if record.LRDetected && result.IsSome && result.Value.Next > record.NextIndex then
            record.Expansion <- { expansion with Num = record.Expansion.Num + 1 }
            record.NextIndex <- result.Value.Next
            memoize memo record.Expansion index result
            record.Result <- result
            resolveItem()
        else
            // we are done trying to expand
            if record.LRDetected then record.Result else result

    let result = resolveItem()

    memo.CallStack <- memo.CallStack.Tail
    forgetLRRecord memo expansion index

    // if there are no LR-processing rules at or above us in the stack, memoize
    let foundLR = memo.CallStack.Any(fun lr -> lr.Involved.Contains expansion.Key)
    if not foundLR then
        memoize memo expansion index result
    //else
        //printf "%d %s: can't memoize, because of lr above\n" index name

    if result.IsNone then
        addError memo index { Key = key; Expected = name; CallStack = memo.CallStack }

    result


and tryGetMemo memo expansion index : (IItem option option) =
    match memo.Table.TryFind expansion.Key with
    | None -> None
    | Some expansionDict ->
    match expansionDict.TryFind expansion.Num with
    | None -> None
    | Some ruleDict -> ruleDict.TryFind index

and tryGetLRRecord memo expansion index =
    match memo.Recursions.TryFind expansion.Key with
    | None -> None
    | Some recordDict -> recordDict.TryFind index

and memoize memo expansion index (item : IItem option) =
    let expansionDict = 
        match memo.Table.TryFind expansion.Key with
        | Some expansionDict -> expansionDict
        | None -> 
        let dict = new ExpansionTable()
        memo.Table.Add(expansion.Key, dict)
        dict

    let ruleDict = 
        match expansionDict.TryFind expansion.Num with
        | Some ruleDict -> ruleDict
        | None ->
        let ruleDict = new RuleTable()
        expansionDict.Add(expansion.Num, ruleDict)
        ruleDict

    ruleDict.[index] <- item

and startLRRecord memo expansion index record =
    let recordDict = 
        match memo.Recursions.TryFind expansion.Key with
        | Some recordDict -> recordDict
        | None -> 
        let rdict = new RecordTable()
        memo.Recursions.Add(expansion.Key, rdict)
        rdict

    recordDict.[index] <- record

and forgetLRRecord memo expansion index =
    match memo.Recursions.TryFind expansion.Key with
    | Some recordDict -> recordDict.Remove index |> ignore
    | None -> ()
      
and addError memo pos error =
    if pos > memo.LastErrorPos then
        memo.LastErrorRecords.Clear()

    if pos >= memo.LastErrorPos then
        memo.LastErrorRecords.Add(error)
        memo.LastErrorPos <- pos