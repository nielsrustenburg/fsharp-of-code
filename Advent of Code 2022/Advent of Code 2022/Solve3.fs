module Solve3

open Input
open FParsec
open System.Linq

let halfString (s:string) = (s[..(s.Length/2)-1],s[(s.Length/2)..])
let sortString (s:string) = Seq.sort s |> Seq.map string |> String.concat ""
let commonElement (a:string,b:string) = (Set.intersect (Set.ofSeq a) (Set.ofSeq b)) 
                                        |> Set.toSeq
                                        |> Seq.head

let priorities = Seq.append (seq {'a' .. 'z' }) (seq{'A' .. 'Z'}) |> Seq.toList
let prioVal (x:char) = (List.findIndex (fun y -> x = y) priorities) + 1

let rucksack = restOfLine false .>> newline |>> halfString
let valueRucksack = rucksack |>> commonElement |>> prioVal
let p1Rucksacks = many valueRucksack


let d3p1 = readDayInputAsSingleString 3 |> runwrap p1Rucksacks |> List.sum
let d3p2 = readDayInputAsSingleString 3 |> fun _ -> "3"