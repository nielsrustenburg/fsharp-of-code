module Solve11

open Input
open Utils

let initialGrid = readDayInput 11 |> array2D |> Array2D.map charToInt
let checkFlash (grid:int[,]) x y = grid[x,y] > 9
let getFlashSet (newGrid:int[,]) = Array2D.filterCoords checkFlash newGrid |> Set.ofSeq

let updateGrid grid (updateSet:Set<int*int>) = Array2D.mapi(fun x y v -> if Set.contains (x,y) updateSet then v+1 else v) grid
let flashTheQueue grid queue= Set.fold (fun accGrid (x,y) -> updateGrid accGrid (Array2D.getNeighbour8Set accGrid x y)) grid queue

let rec flashUntilDone grid (flashSet: Set<int*int>) (flashQueue:Set<int*int>) = 
    if flashQueue.IsEmpty
    then (grid, flashSet.Count)
    else flashTheQueue grid flashQueue |> fun newGrid -> getFlashSet newGrid |> fun newFlashSet -> flashUntilDone newGrid newFlashSet (Set.difference newFlashSet flashSet) 
    
let resetCharges grid = Array2D.map (fun v -> if v > 9 then 0 else v) grid

let nextStep grid = 
    Array2D.map (fun charge -> charge+1) grid |>
    fun newGrid -> getFlashSet newGrid |> fun flashSet -> flashUntilDone newGrid flashSet flashSet 

let rec iterateN N grid flashCount = 
    if N = 0 
    then (grid, flashCount) 
    else nextStep grid |> fun (tempGrid, newFlashes) -> resetCharges tempGrid |> fun newGrid -> iterateN (N-1) newGrid (flashCount+newFlashes)

let isSynced grid = Seq.cast<int> grid |> Seq.sum = 0  

let rec iterateUntilSync grid i = 
    if isSynced grid
    then i
    else nextStep grid |> fun (tempGrid, newFlashes) -> resetCharges tempGrid |> fun newGrid -> iterateUntilSync newGrid i+1
        

let d11p1 = iterateN 100 initialGrid 0
let d11p2 = iterateUntilSync initialGrid 0