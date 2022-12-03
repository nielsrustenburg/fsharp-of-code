module Solve3

open Input
open FParsec

let halfString (s:string) = (s[..(s.Length/2)-1],s[(s.Length/2)..])
let sortString (s:string) = Seq.sort s |> Seq.map string |> String.concat ""
let commonElement (a:string,b:string) = (Set.intersect (Set.ofSeq a) (Set.ofSeq b)) 
                                        |> Set.toSeq
                                        |> Seq.head

let priorities = Seq.append (seq {'a' .. 'z' }) (seq{'A' .. 'Z'}) |> Seq.toList
let prioVal (x:char) = (List.findIndex (fun y -> x = y) priorities) + 1

let p1rucksack = restOfLine false .>> newline |>> halfString
let valueRucksack = p1rucksack |>> commonElement |>> prioVal
let p1Rucksacks = many valueRucksack

let d3p1 = readDayInputAsSingleString 3 |> runwrap p1Rucksacks |> List.sum


let findBadge (a:string,b:string,c:string) = Set.intersectMany [Set.ofSeq a; Set.ofSeq b; Set.ofSeq c]
                                                |> Set.toSeq
                                                |> Seq.head

let p2threeElves = tuple3 (restOfLine true) (restOfLine true) (restOfLine false .>> newline)
let p2ValueOfThree = p2threeElves |>> findBadge |>> prioVal
let p2ValueOfAll = many p2ValueOfThree

let d3p2 = readDayInputAsSingleString 3 |> runwrap p2ValueOfAll |> List.sum