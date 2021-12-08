module Solve6

open System
open Input

let findByKey (fish: seq<int * int64>) n = Seq.tryFind (fun x -> fst x = n) fish |> fun x -> 
    match x with
    | Some y -> snd y
    | None -> 0

let calcNextN (fish: seq<int * int64>) n = 
    match n with
    | 0 -> (0, findByKey fish 1)
    | 1 -> (1, findByKey fish 2)
    | 2 -> (2, findByKey fish 3)
    | 3 -> (3, findByKey fish 4)
    | 4 -> (4, findByKey fish 5)
    | 5 -> (5, findByKey fish 6)
    | 6 -> (6, (findByKey fish 7) + (findByKey fish 0))
    | 7 -> (7, findByKey fish 8)
    | 8 -> (8, findByKey fish 0)
    | _ -> raise (Exception("unknown fish detected"))

let toLongFish (l:list<int*int>) = List.map (fun x -> (fst x, int64(snd x))) l

let calcNextState (fish: list<int * int64>) = (Seq.map (fun x -> calcNextN fish x) {0 .. 8}) |> Seq.toList
let calcAfterNIterations n (fish: list<int * int64>) = Seq.fold (fun (acc:list<int*int64>) y -> calcNextState acc) fish {0..(n-1)}
    

let d6p1 = readDayInputAsCSVInt 6 |> Seq.countBy (fun x->x) |> Seq.toList |> toLongFish |> calcAfterNIterations 80 |> List.sumBy (fun x -> snd x)
let d6p2 = readDayInputAsCSVInt 6 |> Seq.countBy (fun x->x) |> Seq.toList |> toLongFish |> calcAfterNIterations 256 |> List.sumBy (fun x -> snd x)