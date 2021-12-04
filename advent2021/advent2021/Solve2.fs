module Solve2

open System
open Input

let createInstructionFunc (dir, count) = 
    match dir with 
    | "forward" -> fun (hor, dep) -> (hor + count, dep)   
    | "down" -> fun (hor, dep) -> (hor,dep + count)   
    | "up" -> fun (hor, dep) -> (hor,dep - count)
    | _ -> raise (Exception("unexpected submarine instruction"))

let createInstructionFunc2 (dir, count) = 
    match dir with 
    | "forward" -> fun (aim, hor, dep) -> (aim, hor + count, dep + (count*aim))   
    | "down" -> fun (aim, hor, dep) -> (aim + count, hor, dep)   
    | "up" -> fun (aim, hor, dep) -> (aim - count, hor, dep)
    | _ -> raise (Exception("unexpected submarine instruction"))

let mapInputLineToInstructionFunc instructionFunc (text: string) = text.Split(' ') |> fun arr -> instructionFunc (arr[0], Int32.Parse arr[1])
let d2p1 = (0,0) |> (readDayInput 2 |> Seq.map (mapInputLineToInstructionFunc createInstructionFunc) |> Seq.fold (fun f g -> f >> g) (fun (a,b) -> (a,b))) |> fun (a,b) -> a*b
let d2p2 = (0,0,0) |> (readDayInput 2 |> Seq.map (mapInputLineToInstructionFunc createInstructionFunc2) |> Seq.fold (fun f g -> f >> g) (fun (a,b,c) -> (a,b,c))) |> fun (a,b,c) -> b*c
