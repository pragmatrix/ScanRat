module ScanRatMatcher

open System;
open System.Collections.Generic;

type Dictionary<'k, 'v> with
    member this.TryFind key =
        match this.TryGetValue key with
        | (true, v) -> Some v
        | (false, _) -> None

type ParseSuccess<'v> = { value: 'v; index: int; next: int }

type ParseFailure = { index: int }

type IItem =
    abstract member next : int;
    abstract member isSuccess : bool;

type ParseResult<'v> =
    | Success of ParseSuccess<'v>
    | Failure of ParseFailure

    interface IItem with
        member this.next 
            with get() =
                match this with
                | Success ps -> ps.next
                | _ -> failwith "no next"
        member this.isSuccess 
            with get() =
                match this with
                | Success _ -> true
                | _ -> false
   
type Key = Object

type Error = { message: string; index: int}

type RuleTable = Dictionary<int, IItem option>
type ExpansionTable = Dictionary<int, RuleTable>

type MemoTable = Dictionary<Key, ExpansionTable>
type Expansion = { key: Key; num : int }

type LRRecord = {
    mutable expansion: Expansion; 
    mutable lrDetected: bool; 
    mutable nextIndex: int; 
    mutable result: IItem option;
    name : string;
    }

type RecordTable = Dictionary<int, LRRecord>
type LRTable = Dictionary<Key, RecordTable>

type Stats = {
    // number of actual production calls
    mutable productions: int;
    // number of successful memo lookups
    mutable memo: int;
    // number of successful lr memo lookupos
    mutable memoLR: int;
    }
    with 
        member this.trackProduction() = this.productions <- this.productions + 1
        member this.trackMemo() = this.memo <- this.memo + 1
        member this.trackMemoLR() = this.memoLR <- this.memoLR + 1


type ErrorRecord = {
    key: Object
    expected: string;
    callStack: LRRecord list; }

type ParsingError = { expected: string; stack: string seq }

type Memo = {
    table: MemoTable; 
    recursions: LRTable;
    mutable callStack: LRRecord list;
    lastErrorRecords: List<ErrorRecord>;
    mutable lastErrorPos: int;
    stats: Stats;
    } 
    with 
        static member create() = {
            table = new MemoTable(); 
            recursions = new LRTable(); 
            callStack = List.empty; 
            lastErrorRecords = new List<_>();
            lastErrorPos = -1;
            stats = { memo = 0; memoLR = 0; productions = 0 }
            }
        member this.lastError 
            with get() : ParsingError seq = 
                let keys = new HashSet<_>()
                seq {
                    for er in this.lastErrorRecords do
                        if not (keys.Contains er.key) then
                            yield { ParsingError.expected = er.expected; stack = List.map (fun lr -> lr.name) er.callStack }
                            List.iter (fun lr -> keys.Add(lr.expansion.key) |> ignore) er.callStack
                            keys.Add(er.key) |> ignore
                }

type IParseContext =
    abstract member memo : Memo with get
    abstract member index : int with get

exception MatcherException of Error

let rec memoCall (context:'c :> IParseContext) (name: string) (production : 'c -> 'r :> IItem) : (IItem option) =
    let memo = context.memo
    let index = context.index
    let key = production :> Key
    let expansion = { key = key; num = 0 }

    match tryGetMemo memo expansion index with
    | Some result -> 
        memo.stats.trackMemo()
        result
    | _ ->

    match tryGetLRRecord memo expansion index with
    | Some record ->
        record.lrDetected <- true
        match tryGetMemo memo record.expansion index with
        | None -> raise (MatcherException({ index = index; message = "Problem with expansion" }))
        | Some result -> 
            memo.stats.trackMemoLR()
            result
    | None ->

    // no lr information

    let recordExpansion = { expansion with num = 1 }
    memoize memo recordExpansion index None

    let record = { 
        LRRecord.lrDetected = false; 
        expansion = recordExpansion; 
        nextIndex = -1; 
        result = None;
        name = name }

    startLRRecord memo expansion index record
    memo.callStack <- record :: memo.callStack

    let rec resolveItem() : IItem option = 

        let pResult = production context :> IItem
        memo.stats.trackProduction()

        let result = if pResult.isSuccess then (Some pResult) else None
        // do we need to keep trying the expansions?
        
        if record.lrDetected && result.IsSome && result.Value.next > record.nextIndex then
            record.expansion <- { expansion with num = record.expansion.num + 1 }
            record.nextIndex <- result.Value.next
            memoize memo record.expansion index result
            record.result <- result
            resolveItem()
        else
            // we are done trying to expand
            if record.lrDetected then record.result else result

    let result = resolveItem()

    memo.callStack <- memo.callStack.Tail
    forgetLRRecord memo expansion index

    // if there are no LR-processing rules at or above us in the stack, memoize
    if not (Seq.exists (fun (r:LRRecord) -> r.lrDetected) memo.callStack) then
        memoize memo expansion index result

    if result.IsNone then
        addError memo index { key = key; expected = name; callStack = memo.callStack }

    result


and tryGetMemo memo expansion index : (IItem option option) =
    match memo.table.TryFind expansion.key with
    | None -> None
    | Some expansionDict ->
    match expansionDict.TryFind expansion.num with
    | None -> None
    | Some ruleDict -> ruleDict.TryFind index

and tryGetLRRecord memo expansion index =
    match memo.recursions.TryFind expansion.key with
    | None -> None
    | Some recordDict -> recordDict.TryFind index

and memoize memo expansion index (item : IItem option) =
    let expansionDict = 
        match memo.table.TryFind expansion.key with
        | Some expansionDict -> expansionDict
        | None -> 
            let dict = new ExpansionTable()
            memo.table.Add(expansion.key, dict)
            dict

    let ruleDict = 
        match expansionDict.TryFind expansion.num with
        | Some ruleDict -> ruleDict
        | None ->
            let ruleDict = new RuleTable()
            expansionDict.Add(expansion.num, ruleDict)
            ruleDict

    ruleDict.[index] <- item

and startLRRecord memo expansion index record =
    let recordDict = 
        match memo.recursions.TryFind expansion.key with
        | Some recordDict -> recordDict
        | None -> 
            let rdict = new RecordTable()
            memo.recursions.Add(expansion.key, rdict)
            rdict

    recordDict.[index] <- record

and forgetLRRecord memo expansion index =
    match memo.recursions.TryFind expansion.key with
    | Some recordDict -> recordDict.Remove index |> ignore
    | None -> ()
      
// we should probably purge all errors at lower positions.

and addError memo pos error =
    if pos > memo.lastErrorPos then
        memo.lastErrorRecords.Clear()

    if pos >= memo.lastErrorPos then
        memo.lastErrorRecords.Add(error)
        memo.lastErrorPos <- pos