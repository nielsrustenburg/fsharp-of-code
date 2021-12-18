module Solve9

open Input
open System

module Array2D =
    let filterCoords f (s:'a[,]) = Seq.map (fun x -> Seq.map (fun y -> (x,y)) {0..(Array2D.length2 s) - 1}) {0..(Array2D.length1 s) - 1} |>
                                    Seq.concat |> Seq.filter (fun (x,y) -> (f s x y))

let charToInt c = int c - int '0'

let getNeighbours arr x y = 
    match (x > 0, y > 0, Array2D.length1 arr - x > 1, Array2D.length2 arr - y > 1) with
    | (true, true, true, true) -> [|(x-1,y);(x,y-1);(x+1,y);(x,y+1)|]
    | (false, true, true, true) -> [|(x,y-1);(x+1,y);(x,y+1)|]
    | (true, false, true, true) -> [|(x-1,y);(x+1,y);(x,y+1)|]
    | (true, true, false, true) -> [|(x-1,y);(x,y-1);(x,y+1)|]
    | (true, true, true, false) -> [|(x-1,y);(x,y-1);(x+1,y)|]
    | (false, false, true, true) -> [|(x+1,y);(x,y+1)|]
    | (true, false, false, true) -> [|(x-1,y);(x,y+1)|]
    | (true, true, false, false) -> [|(x-1,y);(x,y-1)|]
    | (false, true, true, false) -> [|(x,y-1);(x+1,y)|]
    | _ -> raise (Exception("WHAT IS GOING ON?!?!?"))

let getNeighbourSet (arr:int[,]) x y = Set.ofArray (getNeighbours arr x y)

let getNeighbourValues arr x y = getNeighbours arr x y |> Array.map (fun (x,y) -> arr[x,y])
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

