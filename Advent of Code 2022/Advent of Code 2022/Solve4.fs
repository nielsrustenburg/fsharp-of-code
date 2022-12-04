module Solve4

open Input
open FParsec

let prange = pint32 .>> pchar '-' .>>. pint32
let pair = prange .>> pchar ',' .>>. prange .>> newline
let fullyContains (aLow:int, aHigh:int) (bLow:int, bHigh:int) = aLow <= bLow && aHigh >= bHigh

let d4p1 = readDayInputAsSingleString 4 |> runwrap (many pair) |> List.filter (fun (a,b) -> (fullyContains a b) || fullyContains b a) |> List.length

let overLap (aLow:int, aHigh:int) (bLow:int, bHigh:int) = aLow <= bLow && aHigh >= bLow
let d4p2 = readDayInputAsSingleString 4 |> runwrap (many pair) |> List.filter (fun (a,b) -> (overLap a b) || overLap b a) |> List.length
