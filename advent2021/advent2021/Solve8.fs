module Solve8

open Input
open System

type Entry =
    {
        Signal : Set<char>[]
        Output : Set<char>[]
    }

let stringToCharSet (s:string) = Set.ofSeq s
let parseEntry (line:string) : Entry = line.Split " | " 
                                        |> fun x -> 
                                            {
                                                Signal = x[0].Split(' ') |> Array.map stringToCharSet;
                                                Output = x[1].Split(' ') |> Array.map stringToCharSet
                                            }

let pickOnePlusRemainder (arr:'a[]) = Seq.map (fun x -> (arr[x], Array.concat [arr[0..x-1]; arr[x+1..arr.Length-1]])) {0..(arr.Length-1)}

let find1 (entry: Entry) = Array.find (fun x -> Set.count x = 2) entry.Signal
let find4 (entry: Entry) = Array.find (fun x -> Set.count x = 4) entry.Signal
let find7 (entry: Entry) = Array.find (fun x -> Set.count x = 3) entry.Signal
let find8 (entry: Entry) = Array.find (fun x -> Set.count x = 7) entry.Signal
let find2 (entry: Entry) = Seq.find (fun (x,rem) -> Set.intersectMany rem |> Set.isEmpty |> not) (pickOnePlusRemainder entry.Signal) |> fst
let find3 (entry: Entry) = find1 entry |> fun one -> Array.find (fun mb3 -> Set.count mb3 = 5 && Set.isSubset one mb3) entry.Signal   
let find9 (entry: Entry) = (find4 entry,find7 entry) |> 
                            fun (four,seven) -> entry.Signal |> 
                                                Array.find (fun mb9 -> 
                                                    Set.count mb9 = 6 &&
                                                    Set.isSubset four mb9 && 
                                                    Set.isSubset seven mb9)
let find5 (entry: Entry) = (find1 entry, find9 entry) |> fun (one,nine) -> entry.Signal |> Array.find (fun mb5 -> 
    Set.count mb5 = 5 &&
    not (Set.isSubset one mb5) && 
    Set.isSubset mb5 nine)
let find6 (entry: Entry) = (find5 entry, find9 entry) |> fun (five,nine) -> entry.Signal |> Array.find (fun mb6 -> 
    Set.count mb6 = 6 &&
    not (mb6 = nine) && 
    Set.isSubset five mb6)
let find0 (entry: Entry) = (find6 entry, find9 entry) |> fun (six,nine) -> entry.Signal |> Array.find (fun mb0 -> Set.count mb0 = 6 && not (mb0 = six) && not (mb0 = nine))

let digitMap entry = 
    Map.empty
        .Add(find0 entry ,0)
        .Add(find1 entry ,1)
        .Add(find2 entry ,2)
        .Add(find3 entry ,3)
        .Add(find4 entry ,4)
        .Add(find5 entry ,5)
        .Add(find6 entry ,6)
        .Add(find7 entry ,7)
        .Add(find8 entry ,8)
        .Add(find9 entry ,9)

let calcEntryOutputDigits entry = digitMap entry |> fun dict -> Array.map (fun x -> dict[x]) entry.Output

let getDatNumber (arr: int[]) = arr[0]*1000+arr[1]*100+arr[2]*10+arr[3]

let prepDatShit = readDayInput 8 |> 
                    Seq.toList |> 
                    List.map parseEntry |>
                    List.map calcEntryOutputDigits

let d8p1 = prepDatShit |>
            Array.concat |>
            Array.countBy id |> 
            Array.fold (fun acc (k,v) -> 
                match k with 
                | 1 | 4 | 7 | 8 -> acc+v
                | _ -> acc) 0

let d8p2 = prepDatShit |> List.sumBy getDatNumber