module MacroExpander

open System
open Expecto.ExpectoFsCheck
open FsCheck


type typeT = Immediate | Register | Instruction | Label

type token = Parameter of int | Other of string | Macro of string

type macroT = {
    Name: string;
    Parameters: typeT list;
    Body: token list list;
}

// type MacroInvT = {
//     Macro: macroT;
//     Parameters: typeT list; 
// }

exception MacroException of string

let testMacroList = [{Name = "Steve"; Parameters = [Register; Immediate]; Body = [[Other "MOV"; Parameter 1; Other ","; Parameter 2]; [Other "MOV"; Parameter 1; Other ","; Parameter 2]] };
                    {Name = "Test"; Parameters = [Immediate]; Body = [[Parameter 1; Macro "Steve R10 #0xF"]]}]

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let rec expandMacro (call: string)(macroList: macroT list)= 
    //tokenise and extract the parameters
    let splitCallIntoWords (line : string) =
        line.Split(([| ' '; '\t'; '\f'; '\r'; '\n'; '\b'; ',' |] : char array),
            System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.collect (function | "" -> [||] | s -> [| s |])
        |> Array.map (fun s -> s.Trim())
        |> Array.toList


    let rec replaceParameters (parameterList) (macroBody: (token list))= 
        let replaceElements (parameterPair: (typeT * int * string)) stringBody =
            let (_,  num, value) = parameterPair
            List.map (fun x ->  match x with
                                | Parameter a when a = num -> Other value
                                | Macro string ->   match (expandMacro string macroList) with 
                                                    | Ok string -> Other string
                                                    | Error _ -> Other "Recursive call problem" //TODO
                                | x -> x
                                 ) stringBody
        match parameterList with
        | Ok lst -> match lst with
                    | [] ->  Ok macroBody
                    |(a, b, c)::t -> replaceElements (a,b, c) macroBody
                                    |> (replaceParameters  (Ok t)) //should be fold?
        | Error e -> Error e

    //    
                                       




    let checkHex (str:string) =
        let allowedHex char =
            (65 <= char && char <= 70) || (97 <= char && char <= 102) || (48 <= char && char <= 57)
        str
        |> Seq.forall ( int >> allowedHex)
    
    let checkBinary (str:string) =
        let allowedBin char =
            (48 <= char && char <= 49)
        str
        |> Seq.forall ( int >> allowedBin) //(fun c -> allowedHex (int c))

    let checkAlphabet (str:string) =
       str
       |> Seq.forall (fun c -> System.Char.IsLetter(c) || c = ' ')



    let typeChecking (parameterList: (typeT * int * string ) list) = 
        let correctTypes = 
            List.map (fun a -> match a with
                               | (Immediate, _, Prefix "#" rest) -> match rest with
                                                                    | Prefix "0x" rest when (checkHex rest) -> true
                                                                    | Prefix "0b" rest when (checkBinary rest) -> true
                                                                    | _ when (rest |> int) >= 0 -> true
                                                                    | _ -> false
                               | (Register, _, Prefix "R" rest) -> (try (rest |> uint32 < uint32(15))
                                                                    with _ -> false)
                               | (Register, _, Prefix "r" rest) -> (try (rest |>uint32 < uint32(15))
                                                                    with  _ -> false)
                               | (Instruction, _,string) when checkAlphabet string -> true
                               | (Label, _, string) when checkAlphabet string -> true
                               | _ -> false) parameterList
        
        match correctTypes with
        | _ when (List.forall (id) correctTypes) -> Ok parameterList
        | _ -> Error "Wrong types" //TOFO find paramater that fails
            

    let splitCall = splitCallIntoWords call
    let macro = List.find ( fun a -> a.Name = splitCall.Item(0) ) macroList


    let parametersForSwap = 
        splitCall
        |>   (function //make a list of only the paramaters
            | _::t when t.Length = macro.Parameters.Length -> Ok t 
            | _ -> Error "Missmatched parameters")
        |> Result.map (List.zip3 macro.Parameters [1..macro.Parameters.Length])//make pairs of the pattern to be replaced and the thing replacing it: this should check types
        |> Result.bind typeChecking

    
    macro.Body
    |> List.map (replaceParameters parametersForSwap)//replace the content of the main from the macro using the list of pairs: tokenise the string and then rebuild it?
    |> List.map  (Result.map (List.map (fun a -> match a with
                                                    | Other a -> a + " "
                                                    | Macro a -> a + " "
                                                    | _ -> failwith "This shouldn't be possible ever" )))
    |> List.map (Result.map System.String.Concat)
    |> (fun lst ->  match lst.Head with
                    | Ok _ -> Ok (System.String.Concat (List.map (fun a -> match a with
                                                                              | Ok str -> "\n" + str
                                                                              | Error e -> e) lst))
                    | Error e -> Error e) //AAAAAAAAAAAAAAA
    //implement good errors
    //still need to go through each line of the body
     //return the completed string sticking spaces back in and new lines


let revOfRevIsOrig (x: int list) = 
        List.rev (List.rev x) = x
    

[<EntryPoint>]
let main argv =

    printfn "Testing with FSCheck"
    Check.Quick revOfRevIsOrig |> ignore
    Console.ReadKey() |> ignore
    
    match expandMacro "Steve R13 #0x2a" testMacroList with
    | Ok str -> printfn "%s" str
    | _ -> printfn "%s" "Error"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
