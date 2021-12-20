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

let filterCaveOptions (options: Set<string>) visited = Set.filter (fun (x:string) -> Char.IsUpper x[0] || not (Set.contains x visited)) options

let rec allPathsAtoB (graph:Map<string,Set<string>>) a b visited = 
    if a = b then [[b]]
    else
        let viableOptions = filterCaveOptions graph[a] visited
        if viableOptions.IsEmpty then []
        else
            let pathsFromNexts = Set.fold (fun acc x -> List.append acc (allPathsAtoB graph x b (visited.Add(a)))) [] viableOptions
            List.map (fun x -> a::x) pathsFromNexts 
            
let d12p1 = readDayInput 12 |> getGraphDict |> fun gr -> allPathsAtoB gr "start" "end" Set.empty |> List.length 
let d12p2 = 0