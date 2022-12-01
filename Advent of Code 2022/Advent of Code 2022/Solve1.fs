module Solve1
open FParsec
open Input
open System

let unwrap x = match x with
                | Success(s,_,_) -> s
                | Failure(err,st,_) -> raise (Exception(st.ToString()))
let runwrap p s = run p s |> unwrap
let pelf = (many1 (pint32 .>> newline)) |>> List.sum
let manyElfs = sepBy pelf newline

let d1p1 = readDayInputAsSingleString 1 |> runwrap manyElfs  |> List.max
let d1p2 = readDayInputAsSingleString 1 |> runwrap manyElfs  |> List.sortDescending |> List.take(3) |> List.sum
