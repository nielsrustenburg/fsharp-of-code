module Solve7

open Input
open System

let getMedianValue (lst : list<int>): int =  ((List.length lst)-1) / 2 |> fun x -> lst[x]
let calculateCrabFuel (f: list<int> -> int) (lst : list<int>) :int = f lst |> fun x -> List.fold (fun (acc:int) n -> acc + (abs (x - n))) 0 lst
let rec fuelCost (n:int): int = if n > 0 then n + fuelCost (n-int(1)) else 0
let calculateCrabFuel2 f lst :int = f lst |> fun x -> List.fold (fun (acc:int) n -> acc + fuelCost (abs (x - n))) 0 lst
let getAverage (lst: list<int>): float = float(List.sum lst) / float(List.length lst)
let getFloorAverage (lst: list<int>) = getAverage lst |> floor |> int
let getCeilAverage (lst: list<int>) = getAverage lst |> ceil |> int

let d7p1 = readDayInputAsCSVInt 7 |> Seq.sort |> Seq.toList |> calculateCrabFuel getMedianValue
let d7p2 = readDayInputAsCSVInt 7 |> Seq.toList |> List.map (fun x -> int(x)) |> fun x -> seq {calculateCrabFuel2 getCeilAverage x; calculateCrabFuel2 getFloorAverage x} |> Seq.min
let test = readDayInputAsCSVInt 7 |> Seq.toList |> List.map (fun x -> int(x)) |> getAverage