module Solve3
open Input

let updateBitCounts countArray current = Array.map2 (fun a b -> if b = '1' then a+1 else a-1) countArray current
let getSameLengthZeroSequence arr = Array.length arr |> Array.zeroCreate 
let calcGammaBits arr = Array.map (fun a -> if a > 0 then '1' else '0') arr

let rec bitsToIntAcc bits = 
    if (Seq.length bits > 0) 
    then
        match (Seq.head bits) with
        | '1' -> bitsToIntAcc (Seq.skip 1 bits) |> fun (a,b) -> (a + pown 2 b, b+1) 
        | _ -> bitsToIntAcc (Seq.skip 1 bits) |> fun (a,b) -> (a,b+1) 
    else (0,0)

let bitsToInt bits = bitsToIntAcc bits |> fun (a,b) -> a

let bitFlip seq = Seq.map (fun a -> if a = '1' then '0' else '1') seq

let powerConsumption gammaBits = bitsToInt gammaBits * (bitFlip gammaBits |> bitsToInt)

let d3p1 = readDayInput 3 |> fun inputLines -> Seq.fold updateBitCounts (Seq.head inputLines |> Seq.toArray |> getSameLengthZeroSequence) (Seq.map (fun a -> a |> Seq.toArray) inputLines) |> calcGammaBits |> powerConsumption

let rec splitOnBitRecursive (bit: int) (seq: seq<string>) =
    if (Seq.length seq > 0)
    then
        splitOnBitRecursive bit (Seq.skip 1 seq) |> fun (ones, zeroes) -> 
            match (Seq.head seq |> fun arr -> arr[bit]) with
            | '1' -> (Seq.head seq |> Seq.singleton |> Seq.append ones, zeroes)
            | _ -> (ones, Seq.head seq |> Seq.singleton |> Seq.append zeroes)     
    else ([],[])

let selShorter (a,b) = if Seq.length a < Seq.length b then a else b
let selLonger (a,b) = if Seq.length b > Seq.length a then b else a

let rec splitOnSmaller (bit: int) (seq: seq<string>) = 
    if(Seq.length seq  = 1) then Seq.head seq else splitOnBitRecursive bit seq |> selShorter |> splitOnSmaller (bit+1)

let rec splitOnLarger (bit: int) (seq: seq<string>) = 
    if(Seq.length seq  = 1) then Seq.head seq else splitOnBitRecursive bit seq |> selLonger |> splitOnLarger (bit+1)

let d3p2 = readDayInput 3 |> fun lines -> (splitOnSmaller 0 lines |> bitsToInt, splitOnLarger 0 lines |> bitsToInt) |> fun (a,b) -> a*b
