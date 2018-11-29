﻿(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Playground
    Description: File for self-contained test code and checking FABLE compiler bugs
*)

module Testbench

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open DP
open Refs
open EEExtensions
open Helpers
open CommonData
open ExecutionTop


let getTBWithTab() =
    Refs.fileTabList
    |> List.map (fun tab -> tab, getCode tab)
    |> List.filter (snd >> String.trim >> String.startsWith "##TESTBENCH")
    |> function | [tab, tb] -> Ok (tab, tb)
                | [] -> Error "No testbench is loaded"
                | _ -> Error "More than one testbench is loaded"

let getTB() = 
    getTBWithTab()
    |> Result.map snd

let parseTbLine lNum (lin:string) =

    let apcsRegs rNumLst =
        let rnd = System.Random()
        rNumLst
        |> List.map register
        |> List.map (fun rn -> rn, rnd.Next(-100,100) |> uint32)

    let (|RESOLVE|_|) lst =
        let ops = 
            String.concat " " lst
            |> String.splitString [|","|] 
            |> Array.map String.trim
            |> Array.toList
        let parseLiteral  = function | LITERALNUMB (lit,"") -> [lit] | _ -> []
        let parseL = List.map parseLiteral ops
        if List.exists ((=) []) parseL then None
        else
            List.concat parseL |> Some
    let (|Defs|_|) words =
        match words with
        | [RegMatch (Ok rn) ; "IS" ; LITERALNUMB (lit,"")] -> (TbRegEquals(lNum, rn,lit)) |> Some
        | RegMatch (Ok rn) :: "PTR" :: RESOLVE lits -> (TbRegPointsTo(lNum, rn, 0u, lits)) |> Some
        | _ -> None
    match lin.ToUpper() |> String.splitOnWhitespace |> Array.toList  with
    | "IN" :: Defs tbSpec -> Ok (TbIn, tbSpec)
    | "OUT" :: Defs tbSpec -> Ok (TbOut, tbSpec)
    | "STACKPROTECT" :: _ -> Ok(TbOut, TbStackProtected 0u)
    | "DATAAREA" :: LITERALNUMB (lit,"") :: _ -> Ok(TbIn, TbSetDataArea lit)
    | "PERSISTENTREGS" :: RESOLVE lits -> Ok(TbIn, APCS (apcsRegs (lits |> List.map int)))
    | _ -> 
        Error (lNum,"Parse Error in testbench")




/// Process test specs in textual order linking allocated data addresses.
/// Initstack - stack initial value.
/// Start - current data address for allocation
let linkSpecs initStack start specs =
    let addSpec (start,linkedSpecs) (inOut,spec) =
        match spec with
        | TbRegEquals(_lNum, rn,u) -> start, (inOut, spec) :: linkedSpecs
        | TbRegPointsTo(lNum, rn, _start, uLst) ->
            let n = start + uint32(uLst.Length*4)
            n, (inOut, TbRegPointsTo(lNum, rn, n, uLst)) :: linkedSpecs
        | TbStackProtected _ -> start, (inOut, TbStackProtected initStack) :: linkedSpecs
        | TbSetDataArea u -> u, (inOut, TbSetDataArea u) :: linkedSpecs
        | APCS regs -> start, (inOut, APCS regs) :: linkedSpecs
    List.fold addSpec (start,[]) specs
    |> snd
    |> List.rev

/// Parse lines defining a single test.
/// Return Result is Test object, or list of line numbers and error messages.
let parseOneTest initStack dataStart testNum lines =

    let checkRes, tbLines =
        lines
        |> List.partition (snd >> String.trim >> String.startsWith ">>")
    
    let testLines,testErrors =
        tbLines
        |> List.filter (snd >> (<>) "")
        |> List.map (fun (i,lin) -> parseTbLine (i+1) lin)
        |> List.splitResult

    match testErrors with
    | [] ->
        let linkedLines = linkSpecs initStack dataStart testLines 
        { 
            TNum = testNum;
            Ins = List.collect (function | (TbIn,x) -> [x] | _ -> []) linkedLines
            Outs = List.collect (function | (TbOut,x) -> [x] | _ -> []) linkedLines
            CheckLines = (checkRes |> List.map snd)
            InitSP = initStack
        } |> Ok
    | errors -> Error errors


    
/// Parse testbench file returning as result list of Tests or errors
let parseTests initStack dStart lines =
    let parseChunk initStack dStart chunk =
        List.head chunk
        |> snd
        |> String.splitOnWhitespace 
        |> Array.toList
        |> (function | "#TEST" :: LITERALNUMB (n,"") :: _ -> parseOneTest initStack dStart (int n) (List.tail chunk)
                     | x -> Error [1, sprintf "Can't parse test header '%A'" (List.truncate 2 x)])
    lines
    |> List.map String.trim
    |> List.indexed
    |> List.filter (snd >> (<>) "")
    |> List.tail
    |> List.chunkAt (snd >> String.startsWith "#TEST")
    |> List.map (fun chunk -> parseChunk initStack dStart chunk)

/// Write test Checklines to the buffer containing the testbench file
let writeTest (test:Test) =
        getTBWithTab()
        |> Result.map ( fun (tabId, dat) ->
            dat
            |> String.splitString [|"\n"|]
            |> Array.toList
            |> List.map String.trim
            |> List.chunkAt (String.trim >> String.startsWith "#TEST")
            |> List.collect (fun chunk -> 
                        let testLst = String.splitOnWhitespace (List.head chunk) |> Array.toList
                        let testData = List.filter (String.trim >> String.startsWith ">>" >> not) (chunk |> List.tail)
                        match testLst with
                        | "#TEST" :: LITERALNUMB (n,"") :: _ when int n = test.TNum -> (sprintf "#TEST %d" n) ::testData @ test.CheckLines
                        | _ -> chunk) // no change
            |> List.filter ((<>) "")
            |> String.concat "\n"
            |> fun r -> tabId, r)
        |> function | Ok (tabId, txt) -> 
                        let editor = editors.[tabId]
                        editor?setValue txt
                    | Error _-> showAlert "Error" "What? can't find testbench to write results!"

/// make the initial dataPath (containing test inputs).
/// test: the Test.
/// rmap: the initial DataPath created from the memory image of the program being tested.
let initTestDP test rMap =
    let rMap' = {rMap with Regs = Map.add R13 test.InitSP rMap.Regs}
    let ldSpecs = [
                    test.Ins |> List.map (fun sp -> TbIn,sp)
                    test.Outs |> List.map (fun sp -> TbOut,sp)
                  ] |> List.concat
    let addSpec dp (inout,spec) =
        match inout,spec with
        | TbOut, TbRegEquals(_, rn,u) -> dp
        | TbIn, TbRegEquals(_, rn,u) -> {dp with Regs = Map.add rn u dp.Regs}
        | tbio,TbRegPointsTo(_, rn, start, uLst) ->
            let mm' = ExecutionTop.addWordDataListToMem start dp.MM (uLst |> List.map Dat)
            let dp' = Map.add rn start dp.Regs
            {dp with Regs = dp'; MM = (match tbio with | TbIn -> mm' | TbOut -> dp.MM)}
        | _, TbStackProtected _u -> dp
        | _, APCS rLst -> 
            let rm = List.fold (fun regs (rn,u) -> Map.add rn u regs) dp.Regs rLst
            {dp with Regs = rm}
        | _, TbSetDataArea u -> dp

    List.fold addSpec rMap' ldSpecs

/// Create a list of errors from a test specification and output DataPath of the tested program.
/// test: the test specification (which will have generated program inputs)
/// dp: the Datapath to check against the specification outputs
let checkTestResults (test:Test) (dp:DataPath) =
    let specs = test.Outs
    let checkSpec spec =
        let checkOneLoc (ma,u) =
            match Map.tryFind (WA ma) dp.MM with
            | Some (Dat u') when u = u' -> []
            | Some (Dat u') -> [u, TbMem (ma, Some u'), spec]
            | _ -> [u, TbMem (ma, None), spec]
        match spec with
        | TbStackProtected sp when dp.Regs.[R13] <> sp -> [0u, TbVal dp.Regs.[R13], spec ]
        | TbStackProtected sp ->
            Map.toList dp.MM
            |> List.filter (fun (WA u, mm) -> u >= sp )
            |> List.collect (fun (WA u, mm) -> match mm with | Dat m -> [u,m] | CodeSpace -> [])
            |> List.map (fun (u,m) -> 0u, TbMem (u, Some m), spec)
        | TbRegEquals(lNum, rn, u) when dp.Regs.[rn] = u -> []
        | TbRegEquals(lNum, rn, u) -> [u, TbVal dp.Regs.[rn], spec]
        | TbRegPointsTo(_, rn, start, uLst) ->
            uLst
            |> List.indexed
            |> List.map (fun (n,u) ->  (start + uint32(4*n), u))
            |> List.collect checkOneLoc
        | APCS rLst -> 
            rLst
            |> List.collect (fun (rn,u') -> match u' = dp.Regs.[rn] with
                                            | true -> [] 
                                            | false -> [u', TbVal dp.Regs.[rn], APCS [rn,u']])
        | _ -> []
    specs |> List.collect checkSpec

/// Generate one Test of result messages and add them to the testbench buffer.
/// If no errors mark the Test as Passed.
/// test: test to add (one of those in the testbench).
/// dp: DataPath after test simulation ends.
/// Returns true if test has passed
let addResultsToTestbench (test:Test) (dp:DataPath) =
    let displayError (u: uint32, check: tbCheck, spec:TbSpec) =
        match check, spec with
        | TbVal spAct, TbStackProtected sp -> 
            sprintf "\t>>- Unbalanced Stack. SP: Actual: %d, Expected: %d" spAct sp
        | TbMem(adr,act), TbStackProtected sp -> 
            let actTxt = match act with None -> "None" | Some a -> sprintf "%d" a
            sprintf "\t>>- Caller Stack [%x] -> Actual: %s." adr actTxt
        | TbVal act, TbRegEquals(n, reg, v) -> 
            sprintf "\t>>- %A: Actual: %d, Expected: %d" reg act v
        | TbMem(adr,act), TbRegPointsTo(n, ptr, start, uLst) -> 
            let actTxt = match act with None -> "None" | Some a -> sprintf "%d" a
            let offset = int (adr - start)
            sprintf "\t>>- [%A,#%d] -> Actual: %s. expected: %d" ptr offset actTxt uLst.[offset/4] 
        | TbVal act, APCS [rn,exptd] -> sprintf "\t>>- Persistent Register %A -> Actual: %d. expected: %d" rn act exptd
        | _ -> failwithf "What?: inconsistent specs and check results"
    let errorLines = 
        checkTestResults test dp
        |> List.map displayError
    let resultLines =
        errorLines
        |> function | [] -> [sprintf ">>; Test %d PASSED." test.TNum]
                    | errMess -> sprintf "\t>>- Test %d FAILED." test.TNum :: errMess
    writeTest {test with CheckLines = resultLines}
    errorLines = []
    
/// Top-level testbench parse. Locate loaded testbench, generate pair of testbench tab ID
/// and Test list, or Error message. If testbench lines contain errors these are highlighted in buffer.
/// Previous error highlights are removed from buffer.
let getParsedTests dStart =

    let processParseErrors (eLst: Result<Test,(int*string) list>list) =
        let highlightErrors tab = 
            Editors.removeEditorDecorations tab
            List.iter (fun (lNum, mess) -> Editors.highlightLine tab lNum "editor-line-error")  
        match getTBWithTab() with
        | Error mess -> Error mess
        | Ok (tab,_) ->
            List.iter (Result.mapError (highlightErrors tab) >> ignore) eLst
            match List.errorList eLst with
            | [] -> List.okList eLst |> Ok
            | _ -> 
                Tabs.selectFileTab tab
                Error "Parse errors in testbench"

    let initStack = 0xFF000000u
    getTBWithTab()
    |> Result.bind (
            fun (tab, tb) -> 
                String.toUpper tb
                |> String.splitString [|"\n"|]
                |> Array.toList
                |> parseTests initStack dStart
                |> processParseErrors
                |> Result.map (fun x -> tab,x))


