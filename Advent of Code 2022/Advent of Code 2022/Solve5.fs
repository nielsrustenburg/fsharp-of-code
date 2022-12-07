module Solve5

open Input
open FParsec

let addToMap (value:char) (label:char) (map:Map<char, char list>) = 
    if value = '0' 
    then map 
    else 
        if Map.containsKey label map 
        then Map.add label (value::(Map.find label map)) map
        else Map.add label (List.singleton value) map

let rec addLayerToMap (layer:char list) (labels: char list) map:Map<char, char list> = 
    match (layer, labels) with 
    | ([lastBox], [lastLabel]) -> addToMap lastBox lastLabel map
    | (h1::t1, hl::tl) -> addLayerToMap t1 tl (addToMap h1 hl map)
    | _ -> failwith "empty or unequal lists"

// TODO: in theory input could start with an empty stack, in which case we still want an empty list added to the map
let rec mapToStacks (map: Map<char, char list>) (stacks: char list list, labels: char list) = 
    match stacks with
    | [x] -> addLayerToMap x labels map
    | x::tail -> addLayerToMap x labels (mapToStacks map (tail,labels) )
    | _ -> failwith "empty list"

let pbox = between (pchar '[') (pchar ']') anyChar
let pempty = pstring "   " >>% '0'
let pline = sepBy1 (pbox <|> pempty) (pchar ' ') 
let pstacks = sepEndBy pline newline
let plabel = between (pchar ' ') (pchar ' ') anyChar
let plabels = sepBy plabel (pchar ' ') .>> newline
let parrangement = pstacks .>>. plabels |>> mapToStacks Map.empty
let pinstruction = tuple3 (pstring "move " >>. pint32) (pstring " from " >>. anyChar) (pstring " to " >>. anyChar)
let pinstructions = sepEndBy pinstruction newline
let pwholeinput = (parrangement .>> newline) .>>. pinstructions
let rec applyInstruction (stacks: Map<char, char list>) (amount: int32, from: char, towards: char) = 
    match (amount, Map.find from stacks, Map.find towards stacks) with
    | (0, _,_) -> stacks
    | (_, hfrom::tfrom, tow) -> applyInstruction (Map.add from tfrom (Map.add towards (hfrom::tow) stacks)) (amount-1, from, towards)
    | _ -> failwith "you donkey"

let rec applyAllInstructions (instructions: (int * char * char) list) (stacks: Map<char, char list>) = 
    match instructions with
    | [ins] -> applyInstruction stacks ins
    | h::t  -> (applyInstruction stacks h) |> applyAllInstructions t
    | _ -> failwith "you buffoon"

let getTopBoxesAsString (stacks: Map<char, char list>) = Map.fold (fun s k v -> (k,List.head v)::s) [] stacks |> List.sortBy (fun (k,v) -> k) |> List.map (fun (k,v) -> v) |> Array.ofList |> System.String

let d5p1 = readDayInputAsSingleString 5 |> runwrap pwholeinput |> fun (x,y) -> applyAllInstructions y x |> getTopBoxesAsString
let d5p2 = "soon"