module Solve2

open FParsec
open Input
open System

let xyz = choice [
            pstring "X"
            pstring "Y"
            pstring "Z"
                ] .>> newline

let points (x,y,z) letter = 
    match letter with
        | "X" -> x
        | "Y" -> y
        | "Z" -> z

let p1 = choice [
     pstring "A " >>. xyz |>> points (4,8,3)
     pstring "B " >>. xyz |>> points (1,5,9)
     pstring "C " >>. xyz |>> points (7,2,6)
]

let p2 = choice [
    pstring "A " >>. xyz |>> points (3,4,8)
    pstring "B " >>. xyz |>> points (1,5,9)
    pstring "C " >>. xyz |>> points (2,6,7)
]

let d2p1 = readDayInputAsSingleString 2 |> runwrap (many p1) |> List.sum
let d2p2 = readDayInputAsSingleString 2 |> runwrap (many p2) |> List.sum