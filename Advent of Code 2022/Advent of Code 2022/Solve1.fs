module Solve1
open FParsec
open Input
open System

let pelf = (many1 (pint32 .>> newline)) |>> List.sum
let manyElfs = sepBy pelf newline

let d1p1 = readDayInputAsSingleString 1 |> runwrap manyElfs  |> List.max
let d1p2 = readDayInputAsSingleString 1 |> runwrap manyElfs  |> List.sortDescending |> List.take(3) |> List.sum
