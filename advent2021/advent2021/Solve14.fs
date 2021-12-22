module Solve14

open FParsec
open Input
open Utils

// Clean up later :DD all code for p1 could be replaced by p2's code, but p2's code is uuuugly

let pinsertion = anyString 2 .>>. (pstring " -> " >>. anyChar)
let parseInput: string * Map<string,char> = readDayInput 14 |> fun seq -> (Seq.head seq, Map.ofSeq (Seq.successful (fun x -> run pinsertion x) seq))
let getSubstrings s = Seq.map2 (fun c1 c2 -> System.String ([|c1;c2|])) s (seq {yield! (Seq.skip 1 s); yield '*'})
let matchRules substring (ruleSet:Map<string,char>) = 
    match ruleSet.TryFind substring with
    | Some(c) -> [|substring[0];c|]
    | None -> [|substring[0]|]

let polymerizeOnce ruleSet polymer = Seq.fold (fun acc ss -> Array.append acc (matchRules ss ruleSet)) [||] (getSubstrings polymer) |> System.String
let polymerizeNTimes N ruleSet polymer = Seq.fold (fun acc _ -> polymerizeOnce ruleSet acc) polymer {0..N-1}
let calcP1 (polymer:string) = Seq.countBy id polymer |> fun groups -> (snd (Seq.maxBy snd groups)) - (snd (Seq.minBy snd groups))

let getResultPairs (ruleSet: Map<string,list<string>>) s =
    match ruleSet.TryFind s with
    | Some(l) -> l 
    | None -> [s]

let pinsertion2 = tuple3 anyChar anyChar (pstring " -> " >>. anyChar) |>> fun (i1, i2, ins) -> (System.String [|i1;i2|], [System.String [|i1;ins|]; System.String [|ins;i2|]])
let parseInput2: string * Map<string,list<string>> = readDayInput 14 |> fun seq -> (Seq.head seq, Map.ofSeq (Seq.successful (fun x -> run pinsertion2 x) seq))
let countsAfterStep (ruleSet: Map<string,list<string>>) (counts: list<(string * int64)>) : list<string * int64> = 
    let unmergedCounts = List.fold (fun acc (k,v) -> List.append acc (List.map (fun y -> (y, v)) (getResultPairs ruleSet k))) [] counts
    List.groupBy fst unmergedCounts |> List.map (fun (k,l) -> (k, List.sumBy snd l))

let getInitialCounts s = getSubstrings s |> Seq.toList |> List.countBy id |> List.map (fun (k,v) -> (k, int64(v))) 
let p2PolymerizeNTimes N ruleSet counts = Seq.fold (fun acc _ -> countsAfterStep ruleSet acc) counts {0..N-1}

let calcP2 (counts: list<string*int64>) = List.map (fun ((k:string),v) -> (k[0], v)) counts |> List.groupBy fst |> List.map (fun (k,l) -> (k, List.sumBy snd l)) |> fun groups -> (snd (Seq.maxBy snd groups)) - (snd (Seq.minBy snd groups))

let d14p1 = parseInput |> fun (polymer, ruleSet) -> polymerizeNTimes 10 ruleSet polymer |> calcP1
let d14p2 = parseInput2 |> fun (polymer, ruleSet) -> getInitialCounts polymer |> fun counts -> p2PolymerizeNTimes 40 ruleSet counts  |> calcP2