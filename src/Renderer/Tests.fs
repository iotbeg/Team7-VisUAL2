﻿module Tests
open System
open System.IO
open ExecutionTop
open Integration
open Errors
open EEExtensions
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Fable.Import.Browser
open Node.Exports
open Fable.PowerPack.Keyboard
open CommonData


let fNone = Core.Option.None

let projectDir = __SOURCE_DIRECTORY__ + @"/../../"

type Flags = {
    FN: bool
    FZ: bool
    FC: bool
    FV: bool
}
   
type DPath = { 
    TRegs : uint32 list ; 
    TFlags : Flags 
    }

type TestSetup = { 
    Before: DPath ; 
    Asm: string; 
    After: DPath option; 
    Name: string
    }

type TestT = OkTests | ErrorTests | BetterThanModelTests

let readFileViaNode (path:string) : string =
    (fs.readFileSync path).toString("utf8")

let writeFileViaNode path str =
    let errorHandler _err = // TODO: figure out how to handle errors which can occur
        ()
    fs.writeFileSync( path, str) |> ignore


let loadStateFile (fName:string) =
    let lines = 
        readFileViaNode fName
        |> fun s -> s.Split('\n')
        |> Array.map (fun s -> s.Trim())
        |> Array.map (fun s -> s.Split([|' ';'\t'|]) |> Array.filter ((<>) ""))
        |> Array.filter (Array.isEmpty >> not)
        |> Array.map Array.toList
        |> Array.toList

    let toDP rLst n z c v =
        let fl f = match f with 
                    |"0" -> false 
                    | "1" -> true 
                    | _ -> failwithf "Parse error expecting '1' or '0' as flag value"
        { 
            TRegs = rLst |> List.map (uint32); 
            TFlags = { FN = fl n; FC = fl c; FZ = fl z; FV = fl v}
        }

    let (|GetASM|_|) lines =
        //printfn "GETASM:%A\n\n" lines
        let n = List.findIndex ((=) ["..."]) lines
        let toStr lst = String.concat " " lst
        (String.concat "\r\n" (lines.[0..n-1] |> List.map toStr) , lines.[n+1..]) |> Some
                       
    let (|Test1|_|) lines =
        //printfn "Test1\n%A\n------------\n" lines
        let (|DP|_|) lines =
            //printfn "DP: %A\n\n" lines
            match lines with
            | ("Regs" :: rLst) :: [ "NZCV"; n; z; c; v ] :: rst -> (toDP rLst n z c v |> Some  , rst) |> Some
            | ["ERROR"] :: rst -> (fNone, rst) |> Some
            | _ -> failwithf "Parse error reading DP"
        match lines with
        | [name] :: DP (Some before, (["..."] :: GetASM (asm,  (DP (after, rst))))) -> 
            Some (
                {
                    Before=before 
                    After=after
                    Asm=asm
                    Name = name
                }, rst
            )
        | _ -> failwithf "Parse error reading file (1)" 
    let rec testN tsts = function
        | [] -> tsts
        | Test1 (tst,rst) -> testN (tst :: tsts) rst
        | _ -> failwithf "Parse error reading file (2)"
    
    
    testN [] lines

let handleTestRunError e (pInfo:RunInfo) (ts: TestSetup) =
    let matchMess actual model =
        List.zip actual model 
        |> List.indexed
        |> List.filter (fun (_, (a,m)) -> a <> m)
        |> List.map (fun (r,(a,m)) -> sprintf "Bad output:R%d is 0x%x should be 0x%x" r (uint64 a) (uint64 m))
        |> String.concat "\n"
 
    let regs = 
        pInfo.dp.Regs
        |> Map.toList
        |> List.sortBy (fun (r,u) -> r.RegNum)
        |> List.map (fun (r,u) -> u)
        |> List.take 15
    

    let flags =
        match pInfo.dp.Fl with
        | {N = n ; C = c; V = v; Z = z} -> {FN=n;FC=c;FV=v;FZ=z}
    match e with
    | EXIT ->
        //printfn "ts.After=%A" ts.After
        match ts.After with
        | Core.Option.None -> BetterThanModelTests, ts, pInfo, "Visual2 runs when VisuAL gives an error?"
        | Some {TRegs=tr ; TFlags=fl} when tr = regs && fl = flags-> 
            OkTests, ts, pInfo,"Ok"
        | Some {TRegs=tr ; TFlags=fl} when tr <> regs -> 
            ErrorTests,ts,pInfo, matchMess regs tr
        | Some {TRegs=tr ; TFlags=fl} when fl <> flags -> 
            ErrorTests,ts,pInfo, sprintf "Flags:\n%A\ndo not match model flags\n%A" flags fl
        | _ -> failwithf "What? Can't happen!"

    | NotInstrMem x -> 
        match ts.After with
        | Core.Option.None -> OkTests, ts, pInfo, "Both VisUAL and VisUAL2 give errors"
        | Some _ -> ErrorTests, ts, pInfo, "Error: trying to execute instruction memory"

    | ``Run time error`` (pos,msg) -> 
        match ts.After with
        | Core.Option.None -> OkTests, ts, pInfo, "Both VisUAL and VisUAL2 give errors"
        | Some _ -> ErrorTests, ts, pInfo, sprintf "Error on line %d: %s" pos msg

    | ``Unknown symbol runtime error`` undefs -> 
        ErrorTests, ts, pInfo, "Unknown symbol runtime error: should never happen!"

let writeResultsToFile fn rt resL =

    let nameOfCode = 
        function | OkTests -> "OKs" 
                 | ErrorTests -> "ERRORs" 
                 | _ -> "BETTERs"

    let fName = projectDir + @"test-results/" + (nameOfCode rt + fn)

    let displayState (ts:TestSetup) (outDp: DataPath) =

        let getFlags (a:Flags) = [ a.FN; a.FZ; a.FC; a.FV]
        let getFlagsActual (a:CommonData.Flags) =  [ a.N; a.Z; a.C; a.V]

        let dispFlags (before:DPath) (after:DataPath) (model:DPath) =
            let dispFlag bf af mf n =
                let bToInt = function | true -> "1" | false -> "0"
                let bfv = bToInt (getFlags bf).[n]
                let afv = bToInt (getFlagsActual af).[n]
                let mfv = bToInt (getFlags mf).[n]
                sprintf "%-11c%11s%11s%11s" "NZCV".[n]  bfv afv (if afv <> mfv then mfv else "")
            "Flag          Before        After        Model" +
            ([0..3]
            |> List.map (dispFlag before.TFlags after.Fl model.TFlags)
            |> String.concat "\n")
            

        let dispReg b model a n =
            let model =
                match a.TRegs.[n] = model.Regs.[register n] with
                | false -> ""
                | true -> sprintf "%d" model.Regs.[register n]
            sprintf "R%-8d%11d%11d%11s" n ts.Before.TRegs.[n] a.TRegs.[n]  model

        let dispRegs b model a =
            "Register  Input      Actual Out    Model Out" +
            ([0..14]
            |> List.map (dispReg b model a)
            |> String.concat "\n")

        match ts.After with
        | Some a -> 
            dispRegs ts.Before outDp a + "\n" +
            dispFlags ts.Before outDp a + "\n"
        | _ -> "Error in model\n"


    let displayTest (tt: TestT, ts:TestSetup,ri:RunInfo,mess:string) =
        "\n----------------------------------\n\n" +
        ts.Name + "\n" +
        mess + "\n" +
        displayState ts ri.dp + "\n" +
        "       ---------ASM----------\n" +
        ts.Asm +
        "\n----------------------------------\n\n"
    //printfn "Writing result file\n%s." fName
    match resL with
    | [] -> if fs.existsSync (U2.Case1 fName) then fs.unlinkSync (U2.Case1 fName)
    | _ -> 
        resL
        |> List.map displayTest
        |> String.concat "\n"
        |> (fun txt -> writeFileViaNode fName  txt)



let processTestResults (fn: string) (res: Map<TestT,(TestT*TestSetup*RunInfo*string) list>) =
    let getNum rt = 
        let resL = Map.findWithDefault rt res []
        writeResultsToFile fn rt resL
        resL.Length

    printfn "Test Results from '%s': Ok: %d ; Better: %d ; Errors: %d" 
        fn (getNum OkTests) (getNum BetterThanModelTests) (getNum ErrorTests)

/// on small test files print more info
let RunEmulatorTest ts size =
    let maxSteps = 1000

    let more = size < 4

    let asm = 
        ts.Asm.Split([|'\r';'\n'|]) 
        |> Array.filter (fun s -> s <> "")
        |> Array.toList

    let lim, indentedCode = reLoadProgram asm

    if more then printfn "\n\nIndented ASM:\n%s\n" (indentedCode |> String.concat "\n")

    let ri = lim |> getRunInfoFromState

    if lim.Errors <> [] then 
        match ts.After with
        | Some _ -> ErrorTests, ts, ri, "Visual2 cannot parse test assembler"
        | fNone -> OkTests, ts, ri, "Visual2 cannot parse assembler: however this test returns an error in the model"
    else
        let dpBefore =
            {ri.dp with 
                Regs = 
                    ts.Before.TRegs 
                    |> List.indexed 
                    |> fun lis -> (15,0u) :: lis
                    |> List.map (fun (n,u) -> inverseRegNums.[n], u)
                    |> Map.ofList
                Fl =
                    match ts.Before.TFlags with
                    | {FC=c;FV=v;FZ=z;FN=n} -> {C=c;V=v;N=n;Z=z}
            }

        let ri' = pTestExecute more maxSteps { ri with dp = dpBefore}

        match ri' with
        | {RunErr = Some e;  dp=dp} as ri' -> handleTestRunError e ri' ts
        | {dp=dp} as ri' -> 
            ErrorTests, ts, ri', sprintf "Test code timed out after %d Visual2 instructions" maxSteps

let runEmulatorTestFile fn =
    let testF = projectDir + @"test-data/" + fn
    let testResults =
        loadStateFile testF
        |> (fun tsLis ->
                printfn "Running test file %s with %d tests..." fn tsLis.Length
                List.map (fun ts -> ts, tsLis.Length) tsLis)
        |> List.map (fun (ts, n) -> RunEmulatorTest ts n )
        |> List.groupBy (fun (rt,_,_,_) -> rt)
        |> Map.ofList

    processTestResults fn testResults

let runAllEmulatorTests () =
    let contents = electron.remote.getCurrentWebContents()
    if not (contents.isDevToolsOpened()) then contents.toggleDevTools()
    let files = 
        fs.readdirSync (U2.Case1 (projectDir + "test-data"))
        |> Seq.toList 
        |> (fun lis ->        
                if List.contains "focus.txt" lis 
                then ["focus.txt"]
                else lis)

    List.iter runEmulatorTestFile files
    printfn "Finished running tests: see './test-results' for result files"
    
 
    






