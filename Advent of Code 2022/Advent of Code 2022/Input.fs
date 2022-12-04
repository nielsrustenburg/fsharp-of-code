module Input

open System.IO
open FParsec
open System

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let readAsString (filePath:string) = 
    File.ReadAllText(filePath)

let readDayInput day = readLines $"./days/{day}.txt"
let readDayInputAsSingleString day = readAsString $"./days/{day}.txt"
let readTestInput day = readLines $"./days/test.txt"

let printLines format lines = 
    for a in lines do
        printfn format a

let printDayInput day = readDayInput day |> printLines "%A"

let readDayInputAsInt day = readDayInput day |> Seq.map System.Int32.Parse
let readDayInputAsCSVInt day = readDayInput day |> fun x -> (Seq.head x).Split ',' |> Seq.map System.Int32.Parse

let unwrap x = match x with
                | Success(s,_,_) -> s
                | Failure(err,st,_) -> raise (Exception(st.ToString()))
let runwrap p s = run p s |> unwrap