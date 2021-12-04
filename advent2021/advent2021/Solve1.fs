module Solve1

open Input

let incrementIfBigger (acc, prev) curr = if prev < curr then (acc+1, curr) else (acc, curr)
let day1SolveSauce (first, restofseq) = Seq.fold incrementIfBigger (0, first) (restofseq) |> fst
let getFirstAndRemainder seq = (seq |> Seq.head, seq |> Seq.skip 1)
let sumsOfThree seq = Seq.map3 (fun a b c -> a + b + c) seq (seq |> Seq.skip 1) (seq |> Seq.skip 2)
let d1p1 = readDayInputAsInt 1 |> getFirstAndRemainder |> day1SolveSauce
let d1p2 = readDayInputAsInt 1 |> sumsOfThree |> getFirstAndRemainder |> day1SolveSauce