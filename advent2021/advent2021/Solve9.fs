module Solve9

open Input
open System
open Utils

let getNeighbourSet (arr:int[,]) x y = Set.ofArray (Array2D.getNeighbours4 arr x y)

let getNeighbourValues arr x y = Array2D.getNeighbours4 arr x y |> Array.map (fun (x,y) -> arr[x,y])
let isLowPoint arr x y = getNeighbourValues arr x y |> Array.forall (fun z -> arr[x,y] < z)
let isLowPointV arr x y _ = isLowPoint arr x y
let calcRiskLevel arr x y v = if isLowPointV arr x y v
                              then arr[x,y]+1 
                              else 0

let rec getExtendedNeighbourSetExcludingNines arr newNb extNb = Set.fold (fun acc (x:int,y:int) -> Set.union acc (getNeighbourSet arr x y)) Set.empty newNb |>
                                                                Set.filter (fun (x,y) -> arr[x,y] < 9) |>
                                                                fun newest -> if (Set.isSubset newest extNb) then extNb else getExtendedNeighbourSetExcludingNines arr (Set.difference newest extNb) (Set.union newest extNb)

//getNeighbours arr x y |> Array.filter (fun tup -> not (Set.contains tup extNb)) |> Set.ofArray
let getBasin arr (x,y) = getExtendedNeighbourSetExcludingNines arr (Set.ofList [(x,y)]) Set.empty

let d9p1 = readDayInput 9 |> array2D |> Array2D.map charToInt |> fun intArray ->  Array2D.mapi (calcRiskLevel intArray) intArray |> Seq.cast<int> |> Seq.sum
let d9p2 = readDayInput 9 |> array2D |> Array2D.map charToInt |> fun arr -> (Array2D.filterCoords isLowPoint arr |> Seq.map (fun lp -> getBasin arr lp)) |> Seq.map (fun basin -> Set.count basin) |> Seq.sortDescending |> Seq.take 3 |> Seq.reduce (fun acc p -> p*acc)

