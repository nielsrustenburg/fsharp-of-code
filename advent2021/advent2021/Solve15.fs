module Solve15

open Input
open System
open Utils

type DumbQueue<'a> = { Queue: list<'a>; CompareF: 'a -> int}
    with 
        member this.Next = 
            let (next,rem) = match this.Queue with 
                                | h::t -> (h, t)
                                | _ -> raise (Exception("Waaat"))
            (next, {Queue = rem; CompareF = this.CompareF})
        member this.AddSeq (seq:seq<'a>)= Seq.fold(fun acc x -> x::acc) this.Queue seq |> List.ofSeq |> List.sortBy this.CompareF |> fun x -> {Queue = x; CompareF = this.CompareF}

let parseInput = readDayInput 15 |> array2D |> Array2D.map charToInt
        
let betterThanCached (map: Map<'a, int>) (key, value) =
    match map.TryFind key with
    | Some(v) when v > value -> true
    | None -> true
    | _ -> false

let memeFindShortest (nbfunc: (int*int) -> seq<(int*int)>) (distFunc: (int*int) -> int) dest (queue: DumbQueue<(int*int)*int>) = 
    let mutable (cache: Map<(int*int), int>) = Map.empty.Add((0,0),0)
    let rec findShortestPath (nbfunc: (int*int) -> seq<(int*int)>) (distFunc: (int*int) -> int) dest (queue: DumbQueue<(int*int)*int>) = 
        let ((current, dist), remQueue) = queue.Next
        if current = dest 
        then dist
        else
            let neighbours = nbfunc current
            let nbWithDist = Seq.map (fun z -> (z, dist+(distFunc z))) neighbours
            let filteredNbWithDist = Seq.filter (betterThanCached cache) nbWithDist |> List.ofSeq
            cache <- Seq.fold (fun acc (k, v) -> acc.Add(k,v)) cache filteredNbWithDist
            let newQueue = remQueue.AddSeq filteredNbWithDist
            findShortestPath nbfunc distFunc dest newQueue
    findShortestPath nbfunc distFunc dest queue

let neighbourFunc (grid: 'a[,]) (x,y): seq<(int*int)> = Array2D.getNeighbours4 grid x y
let distFunc (grid: int[,]) (x,y) = grid[x,y]

let createLargeGrid (grid: int[,]) = 
    let (l1, l2) = (Array2D.length1 grid, Array2D.length2 grid)
    let initializer x y = 
        let v = (grid[x % l1,y % l2] + (x / l1) + (y / l2))
        if v < 10 then v
        else (v % 10) + 1
    Array2D.init (l1 * 5) (l2 * 5) initializer
    

let d15p1 = 
    let grid = parseInput
    let start = (0,0)
    let dest = (Array2D.length1 grid - 1, Array2D.length2 grid - 1)
    memeFindShortest (neighbourFunc grid) (distFunc grid) dest {Queue = [(start,0)]; CompareF = snd}

let d15p2 = 
    let babygrid = parseInput
    let grid = createLargeGrid babygrid
    let start = (0,0)
    let dest = (Array2D.length1 grid - 1, Array2D.length2 grid - 1)
    memeFindShortest (neighbourFunc grid) (distFunc grid) dest {Queue = [(start,0)]; CompareF = snd}
