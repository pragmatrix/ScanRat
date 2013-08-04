module InstantMatcher

open System;
open System.Collections.Generic;

type Item = { startIndex : int; nextIndex: int; result : Object option}
    
type Key = Object

type Error = { message: string; index: int}

type Result = 
    | ItemResult of Item 
    | ErrorResult of Error

type RuleTable = Dictionary<int, Item option>
type ExpansionTable = Dictionary<int, RuleTable>

type MemoTable = Dictionary<Key, ExpansionTable>
type Expansion = { key: Key; num : int }

type LRRecord = {
    mutable expansion: Expansion; 
    mutable expansions: int; 
    mutable lrDetected: bool; 
    mutable nextIndex: int; 
    mutable result: Item option
    }

type RecordTable = Dictionary<int, LRRecord>
type LRTable = Dictionary<Key, RecordTable>
type Memo = {
    table: MemoTable; 
    recursions: LRTable;
    callStack: Stack<LRRecord>;
    errorMsgs: List<unit -> string>;
    mutable lastErrorPos: int;
    } 
    with static member create() = {
            table = new MemoTable(); 
            recursions = new LRTable(); 
            callStack = new Stack<LRRecord>(); 
            errorMsgs = new List<unit -> string>();
            lastErrorPos = -1;
            }


exception MatcherException of Error

type Production = { key: Object; f: Memo -> int -> Item option }

type Dictionary<'k, 'v> with
    member this.TryFind key =
        match this.TryGetValue key with
        | (true, v) -> Some v
        | (false, _) -> None

let rec memoCall memo (production : Production) index : (Item option) =
    let expansion = { key = production.key; num = 0 }

    match tryGetMemo memo expansion index with
    | Some result -> 
        result
    | _ ->

    match tryGetLRRecord memo expansion index with
    | Some record ->
        record.lrDetected <- true
        match tryGetMemo memo record.expansion index with
        | None ->
            raise (MatcherException({ index = index; message = "Problem with expansion" }))
        | Some result ->
            result
    | None ->

    // no lr information

    let recordExpansion = { expansion with num = 1 }
    let record = { LRRecord.lrDetected = false; expansions = 1; expansion = recordExpansion; nextIndex = -1; result = None }
    memoize memo recordExpansion index None
    startLRRecord memo expansion index record
    
    memo.callStack.Push record

    let rec resolveItem() : Item option = 
        let mutable result = production.f memo index
        // do we need to keep trying the expansions?
        if record.lrDetected && result.IsSome && result.Value.nextIndex > record.nextIndex then
            record.expansions <- record.expansions + 1
            record.expansion <- { expansion with num = record.expansions }
            record.nextIndex <- result.Value.nextIndex
            memoize memo record.expansion index result
            record.result <- result
            resolveItem()
        else
            // we are done trying to expand
            if record.lrDetected then
                result <- record.result
            forgetLRRecord memo expansion index
            // if there are no LR-processing rules at or above us in the stack, memoize
            memo.callStack.Pop() |> ignore

            if not (Seq.exists (fun (r:LRRecord) -> r.lrDetected) memo.callStack) then
                memoize memo expansion index result

            if result.IsNone then
                addError memo index (fun () -> "expected " + expansion.key.ToString())

            result

    resolveItem()

and tryGetMemo memo expansion index : (Item option option) =
    match memo.table.TryFind expansion.key with
    | None -> None
    | Some expansionDict ->
    match expansionDict.TryFind expansion.num with
    | None -> None
    | Some ruleDict ->
    match ruleDict.TryFind index with
    | Some item -> Some item
    | _ -> None

and tryGetLRRecord memo expansion index =
    match memo.recursions.TryFind expansion.key with
    | None -> None
    | Some recordDict ->
    match recordDict.TryFind index with
    | None -> None
    | Some lrRecord -> Some lrRecord

and memoize memo expansion index (item : Item option) =
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
    ()

and startLRRecord memo expansion index record =
    let recordDict = 
        match memo.recursions.TryFind expansion.key with
        | Some recordDict -> recordDict
        | None -> 
            let rdict = new RecordTable()
            memo.recursions.Add(expansion.key, rdict)
            rdict

    recordDict.[index] <- record
    ()

and forgetLRRecord memo expansion index =
    match memo.recursions.TryFind expansion.key with
    | Some recordDict -> recordDict.Remove index |> ignore
    | None -> 
        ()
      
and addError memo pos messageFunc =
    if pos > memo.lastErrorPos then
        memo.errorMsgs.Add messageFunc
        memo.lastErrorPos <- pos
