module Solve12

open Input
open System

let parseConnection (s:string) = s.Split '-' 

let updateGraphDict (dict:Map<string,Set<string>>) a b = 
    match (dict.ContainsKey a, dict.ContainsKey b) with
    | (true, true) -> dict.Add (a, (dict[a].Add b)) |> fun d -> d.Add (b, (d[b].Add a))
    | (true, false) -> dict.Add (a, (dict[a].Add b)) |> fun d -> d.Add (b, (Set.singleton a))
    | (false, true) -> dict.Add (a, (Set.singleton b)) |> fun d -> d.Add (b, (d[b].Add a))
    | (false, false) -> dict.Add (a, (Set.singleton b)) |> fun d -> d.Add (b, (Set.singleton a))

let getGraphDict (connections: seq<string>): Map<string,Set<string>> = 
    Seq.map parseConnection connections |> 
    Seq.fold (fun acc x -> updateGraphDict acc x[0] x[1]) Map.empty

let filterCaveOptions (options: Set<string>) visited = Set.filter (fun (x:string) -> Char.IsUpper x[0] || not (List.contains x visited)) options
let filterCaveOptions2 (options: Set<string>) visited = 
    let visitCounts = List.countBy (fun y -> y) visited
    let visitedSmallCaveTwice = List.exists (fun z -> (Char.IsLower (string(fst z)[0])) && (snd z) = 2) visitCounts
    Set.filter 
        (fun (x:string) -> Char.IsUpper x[0] || not (List.exists (fun y -> x = fst y) visitCounts) || 
                            (not (x = "start") && not (x = "end") && 
                                (not (visitedSmallCaveTwice)))) options

let rec allPathsAtoB (f: Set<string> -> List<string> -> Set<string>) (graph:Map<string,Set<string>>) a b visited = 
    let newVisited = (a::visited)
    if a = b 
    then [[b]]
    else
        let viableOptions = f graph[a] newVisited
        if viableOptions.IsEmpty then []
        else
            let pathsFromNexts = Set.fold (fun acc x -> List.append acc (allPathsAtoB f graph x b newVisited)) [] viableOptions
            List.map (fun x -> a::x) pathsFromNexts
          
let d12p1 = readDayInput 12 |> getGraphDict |> fun gr -> allPathsAtoB filterCaveOptions gr "start" "end" [] |> List.length 
let d12p2 = readDayInput 12 |> getGraphDict |> fun gr -> allPathsAtoB filterCaveOptions2 gr "start" "end" [] |> List.length 