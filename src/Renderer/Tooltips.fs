(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Tooltips
    Description: Code to implement tooltips and dynamic graphical popups
*)

/// F# References to static parts of renderer DOM
module Tooltips
open System
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open Fable.Core.JsInterop

open Microsoft.FSharp.Collections
open Node.Exports
open EEExtensions
open Refs



// ***********************************************************************************************
//                                  SVG Graphics using React
// ***********************************************************************************************

(*
// SVG elements
let inline svg b c = svgEl "svg" b c
let inline circle b c = svgEl "circle" b c
let inline clipPath b c = svgEl "clipPath" b c
let inline defs b c = svgEl "defs" b c
let inline ellipse b c = svgEl "ellipse" b c
let inline g b c = svgEl "g" b c
let inline image b c = svgEl "image" b c
let inline line b c = svgEl "line" b c
let inline linearGradient b c = svgEl "linearGradient" b c
let inline mask b c = svgEl "mask" b c
let inline path b c = svgEl "path" b c
let inline pattern b c = svgEl "pattern" b c
let inline polygon b c = svgEl "polygon" b c
let inline polyline b c = svgEl "polyline" b c
let inline radialGradient b c = svgEl "radialGradient" b c
let inline rect b c = svgEl "rect" b c
let inline stop b c = svgEl "stop" b c
let inline text b c = svgEl "text" b c
let inline tspan b c = svgEl "tspan" b c


    type SVGAttr =
        | ClipPath of string
        | Cx of obj
        | Cy of obj
        | D of string
        | Dx of obj
        | Dy of obj
        | Fill of string
        | FillOpacity of obj
        | FontFamily of string
        | FontSize of obj
        | Fx of obj
        | Fy of obj
        | GradientTransform of string
        | GradientUnits of string
        | Height of obj
        | MarkerEnd of string
        | MarkerMid of string
        | MarkerStart of string
        | Offset of obj
        | Opacity of obj
        | PatternContentUnits of string
        | PatternUnits of string
        | Points of string
        | PreserveAspectRatio of string
        | R of obj
        | Rx of obj
        | Ry of obj
        | SpreadMethod of string
        | StopColor of string
        | StopOpacity of obj
        | Stroke of string
        | StrokeDasharray of string
        | StrokeLinecap of string
        | StrokeMiterlimit of string
        | StrokeOpacity of obj
        | StrokeWidth of obj
        | TextAnchor of string
        | Transform of string
        | Version of string
        | ViewBox of string
        | Width of obj
        | X1 of obj
        | X2 of obj
        | X of obj
        | XlinkActuate of string
        | XlinkArcrole of string
        | XlinkHref of string
        | XlinkRole of string
        | XlinkShow of string
        | XlinkTitle of string
        | XlinkType of string
        | XmlBase of string
        | XmlLang of string
        | XmlSpace of string
        | Y1 of obj
        | Y2 of obj
        | Y of obj

*)

//---------------------------------------------------------------------------------------------------------------------------
//
// Very Basic DSL for Writing React SVG in F#
//
// React SVG attribute constructors usually need qualified names (SVGAttr.X) to disambiguate from HTML attribute constructors
// The SVG diagram is converted from React to HTML at top-level
//
// Attributes not available as constructors can be made manually:
// SVGAttr.Attr1Name "attr-val" => !!("attr-name", "attr-value")
//
// SVG elements not available as inline constructors can be made using svgEl, e.g. "g" element:
// svgEl "g" [attribute list] [react element list]
//
//---------------------------------------------------------------------------------------------------------------------------

open Fable.Helpers.React
open Fable.Helpers.React.Props
open System.Diagnostics
open System

let lineTipsClickable = false

let arrowMarker mId color =
    defs [] [
                svgEl "marker" [
                    !!("id", mId); 
                    !!("markerWidth","5");
                    !!("markerHeight","5");
                    !!("refX","0");
                    !!("refY","1.5");
                    !!("orient","auto");
                    !!("markerUnits","strokeWidth")
                    SVGAttr.Stroke color
                ] [ path [ D "M0,0 L0,3 L4.5,1.5 z"; SVGAttr.Fill color] [] ]
            ]

/// Include all markers used here for SVG diagrams
/// This function must be inserted in SVG just once before other descriptions.
let svgMarkerDefs() =
    svgEl "g" [] [
        arrowMarker "arrowHead-black" "black"
        arrowMarker "arrowHead-red" "red"
        ]

let arrow color (x1,y1) (x2,y2) =
    let head = 1.
    let al = sqrt((x1-x2)**2. + (y1-y2)**2.)
    let headX = head * (x2-x1) / al
    let headY = head * (y2-y1) / al
    let fS f = sprintf "%.2f" f
    line [
            X1  (fS x1) ; 
            Y1  (fS y1); 
            X2 (fS (x2-headX)); 
            Y2  (fS (y2-headY)); 
            SVGAttr.StrokeWidth (fS (head/5.));  
            SVGAttr.Stroke color; 
            SVGAttr.MarkerEnd (sprintf "url(#arrowHead-%s)" color)
         ] []

let arrowCurve pathCmds =
    path [D pathCmds; SVGAttr.Stroke "black"; SVGAttr.Fill "transparent"; SVGAttr.MarkerEnd "url(#arrowHead)" ] []

let textInBox (width,height) (boxClass:string) (txtClass:string) (rhTopX,rhTopY) txt =
    let fS f = sprintf "%.2f" f
    svgEl "g" [] [
        rect [ 
            X (fS rhTopX)
            Y (fS rhTopY)
            SVGAttr.Height (fS height)
            SVGAttr.Width (fS width)
            !!("dominantBaseline","middle") //align vertically on centre
            SVGAttr.TextAnchor "middle" // align horizontally on centre
            !!("className", boxClass)
        ] []
        text [ 
            X (rhTopX+width/2.0 |> fS); 
            Y (rhTopY+height/2.0 |> fS) ; 
            !!("dominantBaseline","middle"); 
            SVGAttr.TextAnchor "middle"
            !!("className", txtClass)
        ] [ ofString txt ]
    ]


let svgText alignX alignY txtClass posX posY txt =
    let fS f = sprintf "%.2f" f
    text [ 
        X (posX |> fS); 
        Y (posY |> fS) ; 
        !!("dominantBaseline",alignY); 
        SVGAttr.TextAnchor alignX
        !!("className", txtClass)
    ] [ ofString txt ]

let labelText = svgText "left" "middle"

let colText = svgText "middle" "bottom"

let register boxClass txtClass (boxW,boxH) (posX,posY) (bits:int list) =
    let box xp yp b =
        let txt = sprintf "%d" b
        textInBox (boxW,boxH) boxClass txtClass (xp,yp) txt
    let boxes =
        bits
        |> List.indexed
        |> List.rev
        |> List.map (fun (n,b) -> 
            let xp = (float n)*boxW + posX
            box xp posY b)
    svgEl "g" [] boxes



let makeHtmlFromSVG re =
    let ele = ELEMENT "div" [] []
    ReactDom.render(re, ele)
    ele

/// generate an SVG diagram for shifts as HTML DOM
let displayShiftDiagram rn (beforeNum, beforeC) (op2Num, op2C, finalC, writeC, alu) (shiftT: DP.ArmShiftType option) shiftNum =
    let boxW,boxH = 2.7, 2.7
    let posX,posY = 11., 10.
    let posLabX = 2.
    let aluW,aluH = 20.,10.
    let posAluX = posX + boxW*16. - aluW/2.
    let posAluY = posY + 50.
    let sepY = 35.
    let sepY' = posAluY + aluH/2. - boxH/2. - posY
    let carryNX = 2
    let posCX = posX - (float carryNX)*boxW
    let boxClass = "tooltip-shift-reg-box"
    let carryBoxClass = "tooltip-shift-carry-box"
    let aluTxtClass = "tooltip-shift-alu-txt"
    let txtClass = "tooltip-shift-reg-txt"
    let makeLabel = labelText txtClass posLabX
    let svgIfTrue b el =
        svgEl "g" []  (if b then el else [])


    let carryBox yp b =
        let txt = sprintf "%d" b
        textInBox (boxW,boxH) carryBoxClass txtClass (posCX,yp) txt

    let arrow' color startN endN = arrow color (boxW/2. + posX + (float startN)*boxW, posY+boxH) (boxW/2. + posX + (float endN)*boxW, posY+sepY)

    let arrowSet startN endN num =
        svgEl "g" [] (List.map (fun i -> arrow' "black" (startN + i) (endN + i)) [0..num-1])

    let getBits num = 
        [31..-1..0] 
        |> List.map (fun n -> match (num &&& (1 <<< n)) with | 0 -> 0 | _ -> 1)

    let reg = register boxClass txtClass (boxW,boxH)

    let arrows =
            match shiftT with
            | Some DP.LSR
            | Some DP.ASR ->
                [
                    svgIfTrue writeC [arrow' "red" (32 - shiftNum) (-carryNX)]
                    arrowSet 0 shiftNum (32-shiftNum)
                    svgIfTrue (shiftT = Some DP.ASR) <| List.map (fun n -> arrow' "black" 0 n) [0..shiftNum-1]
                ]
            | Some DP.LSL -> 
                [
                    svgIfTrue writeC [arrow' "red" (shiftNum-1) (-carryNX)]
                    arrowSet shiftNum 0 (32 - shiftNum)
                ]
            | Some DP.ROR ->
                [
                    svgIfTrue writeC [arrow' "red" (32 - shiftNum) (-carryNX)]
                    arrowSet 0 shiftNum (32-shiftNum)
                    arrowSet (32-shiftNum) 0 shiftNum
                ]

            | None -> // RRX
                [
                    svgIfTrue writeC [arrow' "red" 31 (-carryNX)]
                    svgIfTrue writeC [arrow' "red" (-carryNX) 0]
                    arrowSet 0 1 31

                ]

    svg
        [ ViewBox "0 0 100 80"; unbox ("width", "600px") ] (
        [      
            svgMarkerDefs() // used to define arrow heads
            carryBox posY beforeC
            carryBox (posY+sepY) op2C
            carryBox (posY+sepY') finalC
            svgIfTrue (not writeC) [arrow' "red" -carryNX -carryNX]
            svgIfTrue (not (alu && writeC)) [arrow "red" (boxW/2. + posCX, posY+sepY+boxH) (boxW/2. + posCX, posY+sepY')]
            svgIfTrue (alu && writeC) [arrow "red" (posAluX, posAluY+aluH/2.) (posCX+boxW, posY+sepY'+boxH/2.)]
            textInBox (aluW,aluH) boxClass aluTxtClass (posAluX,posAluY) "ALU"
            reg (posX,posY) (getBits (beforeNum |> int))
            reg (posX,posY+sepY) (getBits op2Num)
            makeLabel (posY + boxH/2.) "In"
            makeLabel (posY + sepY + boxH/2.) "Op2"
            makeLabel (posY + sepY' + boxH/2.) "Out"
            colText txtClass (posCX + boxW/2.) (posY - 1.) "C"
        ] @ arrows)       
    |> makeHtmlFromSVG




let demoSVG ()  =
    svg 
      [ ViewBox "0 0 100 100"; unbox ("width", "700x") ]
      [ circle 
          [ Cx (!! "50"); Cy (!! "50"); R (!! "45"); !! ("fill", "#0B79CE") ] 
          []
        text [X "25"; Y "25" ] [ofString "a"]
        text [X "50"; Y "50"] [ofString "b"]
        svgMarkerDefs()
        line [X1  "50"; Y1  "50"; X2 "25"; Y2  "25"; SVGAttr.Stroke "red"; SVGAttr.MarkerEnd "url(#arrow)"] []
        rect [X "40.5"; Y "40.5"; SVGAttr.Width "50"; SVGAttr.Height "20"; SVGAttr.Stroke "red"; SVGAttr.Fill "white"; !!("className","tooltip-shift-reg-box")] []
        text [ X "65"; Y "50" ; !!("dominantBaseline","middle"); SVGAttr.TextAnchor "middle"; !!("className","tooltip-shift-reg-txt")] [ ofString "1" ]
      ]
      



   


// ***********************************************************************************************
//                                  Tooltips using Tippy.js
// ***********************************************************************************************

/// top-level function from tippy.js to make tooltips
let tippy(rClass:string, tippyOpts:obj):unit = importDefault "tippy.js"

/// Position of widget on editor buffer character grid.
/// AboveBelow => offset form position, Exact => centered on position
type WidgetPlace =
    | AboveBelow of HPos: int * VPos: int
    | Exact of HPos: int * VPos: int

/// <summary> Make an editor content widget to display over editor. 
/// Position in editor: pos. HTML to display: dom.</summary>
/// <param name="name"> Widget id (tracked by editor) name</param>
/// <param name="dom"> HTML to display in widget </param>
/// <param name="pos">  Position of widget on editor character grid </param>
let makeContentWidget (name: string) (dom:HTMLElement) (pos:WidgetPlace) =
    let h,v = match pos with | AboveBelow(h,v) -> (h,v) | Exact(h,v) -> (h,v)
    let widget = createObj  [
                  "domNode" ==> dom
                  "getDomNode" ==> fun () -> dom
                  "getId" ==> fun () -> name
                  "getPosition" ==> 
                       fun () -> 
                        createObj [ "position" ==>  
                                        createObj [
                                             "lineNumber" ==> v
                                             "column" ==> h
                                        ]
                                    "preference" ==>   
                                        match pos with 
                                        | Exact _ -> [|0|]
                                        | AboveBelow _ -> [|1;2|]
                                  ]
                  ] 
    editors.[currentFileTabId]?addContentWidget widget |> ignore
    currentTabWidgets <- Map.add name widget currentTabWidgets 

/// delete content widget with ID = name on current editor.
let deleteContentWidget name =
    match Map.tryFind name currentTabWidgets with
    | None -> ()
    | Some w ->
        editors.[currentFileTabId]?removeContentWidget w |> ignore
        currentTabWidgets <- Map.remove name currentTabWidgets

let deleteAllContentWidgets() =
    Array.iter deleteContentWidget (Map.keys currentTabWidgets) 

let tippyTheme() =
    match vSettings.EditorTheme with
    | "one-light-pro" | "solarised-light" -> "dark"
    | _ -> "light"

/// <summary> Tooltip generator using tippy.js https://atomiks.github.io/tippyjs/ </summary>
/// <param name="theme"> Tippy theme as defined in CSS: dark, light, etc</param>
/// <param name = "placement"> Tippy placement (top, bottom, etc) </param>
/// <param name = "clickable"> true => click to display, false => hover to display </param>
/// <param name = "button"> true => delay tooltip and make it close on click </param>
/// <param name = "domID"> ID of DOM element to which tooltip is attached </param>
let makeTooltip (theme:string) (placement:string) (clickable:bool) (button:bool) (domID:string) (tooltip: HTMLElement) =
    tippy( "#"+domID, createObj <| 
        [ 
            "html" ==> tooltip 
            "hideOnClick" ==> if clickable then true :> obj else button :> obj
            "interactive" ==> not clickable
            "delay" ==> if button then "[1300,0]" else "0"
            "arrow" ==> true
            "trigger" ==> if clickable then "click" else "mouseenter"
            "arrowType"==> "round"
            "theme" ==> theme
            "placement" ==> placement
        ])

        
/// <summary>
/// Make an info button with associated hover tooltip.
/// </summary>
/// <param name = "h"> horizontal char position for LH edge of button in editor </param>
/// <param name = "v"> line number in editor buffer on which to place button (starting from 0 = top)</param>
/// <param name = "buttonText"> label on button</param>
/// <param name = "toolTipDOM"> DOM to display inside tooltip box </param>
let makeEditorInfoButtonWithTheme theme (clickable:bool) h v (buttonText:string) (toolTipDOM:HTMLElement) = 
    /// Ratio of char width / char size for editor buffer font.
    /// TODO: work this out properly from a test
    let editorFontWidthRatio = 0.6 // works OK for Fira Code Mono
    let name = buttonText.ToLower()
    let domID = sprintf "info-button-%s-%d" name v
    let tooltip = ELEMENT "DIV" [sprintf "tooltip-%s" name] [toolTipDOM]
    let dom = 
        ELEMENT "BUTTON" [ sprintf "info-button-%s" name] [] 
        |> INNERHTML buttonText 
        |> ID domID
        |> STYLE ("margin-left",sprintf "%.0fpx" (editorFontWidthRatio * (float h+2.0) * float (int vSettings.EditorFontSize)))
    dom.addEventListener_click( fun _ ->
        Browser.console.log (sprintf "Clicking button %s" buttonText) |> ignore
        )
    deleteContentWidget domID // in some cases we may be updating an existing widget
    makeContentWidget domID dom <| Exact(0,v)
    makeTooltip theme "bottom" clickable false domID tooltip

let makeEditorInfoButton clickable h v = makeEditorInfoButtonWithTheme (tippyTheme()) clickable h v

/// Add all the static tooltip information on the editor
let addFixedToolTips() =

    let makeTT domID tooltip = makeTooltip "dark" "right" false false domID tooltip
 
    let makeClickTT domID tooltip = makeTooltip "dark" "bottom" true false domID tooltip
    
    let makeTextTT placement htmlID cssClassLst text = makeTooltip "dark" placement false false htmlID (ELEMENT "p" cssClassLst [] |> INNERHTML text)
    let makeButtonTT placement htmlID cssClassLst text = makeTooltip "dark" placement false true htmlID (ELEMENT "p" cssClassLst [] |> INNERHTML text)

    makeTextTT "top" "flags" ["tootip-fixed"] "ARM Status bits (Flags) NZCV. <br> Blue indicates that Flag was written by <br> the most recently executed instruction."
    makeTextTT "bottom" "clock-symbol" ["tootip-fixed"] "Execution time"
    makeTextTT "bottom" "clock-time" ["tootip-fixed"] "Number of <br> Instructions"
    makeTextTT "bottom" "BR15" ["tootip-fixed"] "R15 (PC) is the Program Counter <br> It cannot be used as a data register"
    makeTextTT "right" "BR14" ["tootip-fixed"] "R14 (LR) is the Link Register <br> It can be used as a data register"
    makeTextTT "right" "BR13" ["tootip-fixed"] "R13 (SP) is the Stack Pointer. <br> It can be used as a data register"
    makeButtonTT "bottom" "tab-sym" ["tootip-fixed"] "Displays symbols (labels) <br> after execution has started"
    makeButtonTT "bottom" "tab-mem" ["tootip-fixed"] "Displays current data memory contents after execution has started <br> Words are added dynamically when they are written"
    makeButtonTT "bottom" "tab-reg" ["tooltip-fixed"] "Displays current register contents"
    makeButtonTT "bottom" "rep-hex" ["tooltip-fixed"] "Switch numeric displays to hexadecimal"
    makeButtonTT "bottom" "rep-bin" ["tooltip-fixed"] "Switch numeric displays to binary. <br> In binary '_' is separator <br> used to make bits more readable <br> optional in assembler literals"
    makeButtonTT "bottom" "rep-dec" ["tooltip-fixed"] "Switch numeric displays to two's complement signed decimal"
    makeButtonTT "bottom" "rep-udec" ["tooltip-fixed"] "Switch numeric displays to unsigned decimal"

    let makeRegTT regID = makeTextTT  "right" ("B"+regID) ["tootip-fixed"] (sprintf "%s is a data register" regID)
    List.iter (fun n -> makeRegTT  (sprintf "R%d" n)) [0..12]

open CommonData   

let makeShiftTooltip (h,v) (dp:DataPath, dpAfter:DataPath, uFAfter:DP.UFlags) (rn:RName) (shiftT:DP.ArmShiftType Option, alu:bool) (shiftAmt:uint32) (op2: DP.Op2) =
    let bToi = function |true -> 1 |false -> 0
    let before = dp.Regs.[rn]|> uint64 |> int64 |> int32
    let (after,uF) = DP.evalOp2 op2 dp 
    let finalC = bToi dpAfter.Fl.C
    let finalFWrite = uFAfter.CU
    let after' = after |> uint64 |> int64 |> int32
    printfn "Making shift tooltip"
    let diagram = displayShiftDiagram rn (before |> uint32, bToi dp.Fl.C) (after', bToi uF.Ca, finalC, finalFWrite, alu) shiftT (shiftAmt |> int)
    makeEditorInfoButtonWithTheme "light" lineTipsClickable h (v+1) "Shift" diagram
    
    

