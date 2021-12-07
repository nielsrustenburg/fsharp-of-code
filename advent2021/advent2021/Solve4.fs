module Solve4
open System
open Input

let parseCommaSeparatedIntList (line: string) = line.Split(',') |> Array.map (fun s -> Int32.Parse s) |> Array.toList
let parseChosenNumbers (rows: list<string>) = parseCommaSeparatedIntList rows[0]

let rec groupBy5 rows = 
    if not (List.isEmpty rows)
    then rows[1..5]::(groupBy5 rows.[6..])
    else []

let bingoRowToIntList (row: string) = row.Split(' ') |> Array.filter (fun s -> not (s = "")) |> Array.map (fun s -> Int32.Parse s) |> Array.toList
let bingoRowsToIntArray rows = List.map (fun a -> bingoRowToIntList a) rows |> List.fold List.append List.empty<int>
let parseBingoBoards (rows: list<string>) = groupBy5 rows[1..] |> List.map bingoRowsToIntArray
let parseInput (rows: list<string>) = (parseChosenNumbers rows, parseBingoBoards rows)
let getNthRow n = {(n*5) .. 1 .. (n*5)+4}
let getNthCol n = {n .. 5 .. n+20}
let getCombinationIndices = Seq.map getNthCol {0 .. 4} |> Seq.append (Seq.map getNthRow {0 .. 4}) |> Seq.toList

let filterIndices (lst: list<'a>) (f: seq<int>) = Seq.map (fun a -> lst[a]) f
let getAllBingoCombinations (bingoboard : list<int>) = List.map (fun (rowOrColIndices : seq<int>) -> filterIndices bingoboard rowOrColIndices) getCombinationIndices
let checkBingo (allBingoCombinations : list<seq<int>>) (nums : list<int>) = List.exists (fun x -> Seq.forall (fun y -> List.contains y nums) x) allBingoCombinations
let findFirstBingo (allBingoCombinations : list<seq<int>>) (nums : list<int>) = Seq.find (fun x -> checkBingo allBingoCombinations nums[0..x]) {5 .. List.length nums} |> fun x -> nums[0 .. x]
let selectFastestBingo (bingoNumsWithBoards : list<list<int> * list<int>>) = List.fold (fun acc x -> if List.length (fst acc) < List.length (fst x) then acc else x) bingoNumsWithBoards[0] bingoNumsWithBoards[1..]
let selectSlowestBingo (bingoNumsWithBoards : list<list<int> * list<int>>) = List.fold (fun acc x -> if List.length (fst acc) > List.length (fst x) then acc else x) bingoNumsWithBoards[0] bingoNumsWithBoards[1..]
let calcBingoScoreBITCH (nums: list<int>, board: list<int>) = List.except nums board |> fun x -> List.sum x * List.last nums
let d4p1 = readDayInput 4 |> Seq.toList |> parseInput |> fun (nums, boards) -> List.map (fun oneBoard -> ((getAllBingoCombinations oneBoard |> findFirstBingo) nums), oneBoard) boards |> selectFastestBingo |> calcBingoScoreBITCH
let d4p2 = readDayInput 4 |> Seq.toList |> parseInput |> fun (nums, boards) -> List.map (fun oneBoard -> ((getAllBingoCombinations oneBoard |> findFirstBingo) nums), oneBoard) boards |> selectSlowestBingo |> calcBingoScoreBITCH