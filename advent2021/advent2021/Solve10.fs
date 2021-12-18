module Solve10
open Input
open System

let points1 (c: char) =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let points2 (c: char) =
    match c with
    | ')' -> int64(1)
    | ']' -> int64(2)
    | '}' -> int64(3)
    | '>' -> int64(4)
    | _ -> raise (Exception("what"))

let calcp2 (stack: list<char>) = List.fold (fun acc x -> (acc*int64(5)) + points2 x) (int64(0)) stack

let (|Valid|_|) (s: list<char>, stack: list<char>) = 
    match (s,stack) with 
    | ('('::t, _) -> Some((t,(')'::stack)))
    | ('['::t, _) -> Some((t,(']'::stack)))
    | ('{'::t, _) -> Some((t,('}'::stack)))
    | ('<'::t, _) -> Some((t,('>'::stack)))
    | (x::t, y::t2) when x = y  -> Some((t,t2))
    | _ -> None

let rec calcBothParts (s: list<char>) (stack: list<char>) = 
    match (s,stack) with 
    | ([],rem) -> (None, Some(calcp2(rem)))
    | Valid (newS, newStack) -> calcBothParts newS newStack
    | (h::_, _) -> (Some(points1 h), None)

let filterP1 (results: seq<(option<int> * option<int64>)>) = 
    results |> Seq.choose (fun (x,y) -> x) |> Seq.sum 
let filterP2 (results: seq<(option<int> * option<int64>)>) = 
    results |> Seq.choose (fun (x,y) -> y) |> Seq.sort |> Seq.toList |> fun x -> x[x.Length/2]

let (d10p1,d10p2) = readDayInput 10 |> Seq.map (fun x -> calcBothParts (Seq.toList x) []) |> fun l -> (filterP1 l, filterP2 l)
