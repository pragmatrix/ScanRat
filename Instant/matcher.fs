module InstantMatcher

open System;
open System.Collections.Generic;

type Item = { startIndex : int; nextIndex: int}
    with static member empty = { startIndex = 0; nextIndex = 0 }

type Error = { message: string; index: int}

type Result = 
    | ItemResult of Item 
    | ErrorResult of Error

type RuleTable = Dictionary<int, Item option>
type ExpansionTable = Dictionary<int, RuleTable>

type MemoTable = Dictionary<string, ExpansionTable>
type Expansion = { name: string; num : int }

type LRRecord = {
    mutable expansion: Expansion; 
    mutable expansions: int; 
    mutable lrDetected: bool; 
    mutable nextIndex: int; 
    mutable result: Item option
    }

type RecordTable = Dictionary<int, LRRecord>
type LRTable = Dictionary<string, RecordTable>
type Memo = {
    table: MemoTable; 
    recursions: LRTable;
    results: Stack<Item option> 
    callStack: Stack<LRRecord>;
    errorMsgs: List<unit -> string>;
    mutable lastErrorPos: int;
    } 
    with static member create() = {
            table = new MemoTable(); 
            recursions = new LRTable(); 
            results = new Stack<Item option>();
            callStack = new Stack<LRRecord>(); 
            errorMsgs = new List<unit -> string>();
            lastErrorPos = -1;
            }


exception MatcherException of Error

type Production = { name: string; f: Memo -> int -> Item seq -> unit}

type Dictionary<'k, 'v> with
    member this.TryFind key =
        match this.TryGetValue key with
        | (true, v) -> Some v
        | (false, _) -> None

let rec getMatch input production =
    let memo = Memo.create();

    try
        let matchedItem = memoCall memo production 0 Seq.empty
        match matchedItem with
        | None -> ErrorResult { message= ""; index= 0}
        | Some item -> ItemResult item
    with
    | MatcherException(e) ->
        ErrorResult e
    | e ->
        ErrorResult { message = e.Message; index= 0 }



and memoCall memo (production : Production) index (args : Item seq) : (Item option) =
    let expansion = { name = makeRuleName production args; num = 0 }

    match tryGetMemo memo expansion index with
    | Some result -> 
        memo.results.Push(result)
        result
    | _ ->

    match tryGetLRRecord memo expansion index with
    | Some record ->
        record.lrDetected <- true
        match tryGetMemo memo record.expansion index with
        | None ->
            raise (MatcherException({ index = index; message = "Problem with expansion" }))
        | Some result ->
            memo.results.Push(result)
            result
    | None ->

    // no lr information

    let recordExpansion = { Expansion.name = expansion.name; num = 1 }
    let record = { LRRecord.lrDetected = false; expansions = 1; expansion = recordExpansion; nextIndex = -1; result = None }
    memoize memo recordExpansion index None
    startLRRecord memo expansion index record
    
    memo.callStack.Push record

    let rec resolveItem() : Item option = 
        production.f memo index args
        let mutable result = memo.results.Pop()
        // do we need to keep trying the expansions?
        if record.lrDetected && result.IsSome && result.Value.nextIndex > record.nextIndex then
            record.expansions <- record.expansions + 1
            record.expansion <- { Expansion.name = expansion.name; num = record.expansions }
            record.nextIndex <- result.Value.nextIndex
            memoize memo record.expansion index result
            record.result <- result
            resolveItem()
        else
            // we are done trying to expand
            if record.lrDetected then
                result <- record.result
            forgetLRRecord memo expansion index
            memo.results.Push result
            // if there are no LR-processing rules at or above us in the stack, memoize
            memo.callStack.Pop() |> ignore

            if not (Seq.exists (fun (r:LRRecord) -> r.lrDetected) memo.callStack) then
                memoize memo expansion index result

            if result.IsNone then
                addError memo index (fun () -> "expected " + expansion.name)

            result

    resolveItem()

and makeRuleName production (args : Item seq)= 
    let argNames = Seq.map(fun arg -> arg.ToString())
    production.name + " " + String.Join(", ", argNames)


and tryGetMemo memo expansion index : (Item option option) =
    match memo.table.TryFind expansion.name with
    | None -> None
    | Some expansionDict ->
    match expansionDict.TryFind expansion.num with
    | None -> None
    | Some ruleDict ->
    match ruleDict.TryFind index with
    | Some item -> Some item
    | _ -> None

and tryGetLRRecord memo expansion index =
    match memo.recursions.TryFind expansion.name with
    | None -> None
    | Some recordDict ->
    match recordDict.TryFind index with
    | None -> None
    | Some lrRecord -> Some lrRecord

and memoize memo expansion index (item : Item option) =
    let expansionDict = 
        match memo.table.TryFind expansion.name with
        | Some expansionDict -> expansionDict
        | None -> 
            let dict = new ExpansionTable()
            memo.table.Add(expansion.name, dict)
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
        match memo.recursions.TryFind expansion.name with
        | Some recordDict -> recordDict
        | None -> 
            let rdict = new RecordTable()
            memo.recursions.Add(expansion.name, rdict)
            rdict

    recordDict.[index] <- record
    ()

and forgetLRRecord memo expansion index =
    match memo.recursions.TryFind expansion.name with
    | Some recordDict -> recordDict.Remove index |> ignore
    | None -> 
        ()
      
and addError memo pos messageFunc =
    if pos > memo.lastErrorPos then
        memo.errorMsgs.Add messageFunc
        memo.lastErrorPos <- pos
