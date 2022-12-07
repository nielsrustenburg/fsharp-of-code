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
let plabels = sepBy plabel (pchar ' ')
let parrangement = pstacks .>>. plabels |>> mapToStacks Map.empty

let d5p1 = readDayInputAsSingleString 5 |> runwrap parrangement

let d5p2 = readDayInputAsSingleString 5 |> fun _ -> 5
