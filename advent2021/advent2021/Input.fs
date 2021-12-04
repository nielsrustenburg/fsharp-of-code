module Input

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let readDayInput day = readLines $"./days/{day}.txt"

let printLines format lines = 
    for a in lines do
        printfn format a

let printDayInput day = readDayInput day |> printLines "%A"

let readDayInputAsInt day = readDayInput day |> Seq.map System.Int32.Parse