module Solve5
open System
open Input

type Coordinate =
    {
        X : int
        Y : int
    }

type VentLine = 
    {
        Origin : Coordinate
        Destination : Coordinate
    }

let parseVentCoords (coords: string) : Coordinate = coords.Split ',' |> fun c -> {X = Int32.Parse c[0]; Y = Int32.Parse c[1]}
let parseVentLine (line: string) : VentLine = line.Split " -> " |> fun x -> { Origin = parseVentCoords x[0]; Destination = parseVentCoords x[1] }
let parseAllVentLines = readDayInput 5 |> Seq.map parseVentLine
let filterOutDiagonals (ventLines : seq<VentLine>) = Seq.filter (fun (l:VentLine) -> l.Destination.X = l.Origin.X || l.Destination.Y = l.Origin.Y ) ventLines
let getDiff (line : VentLine) : Coordinate = {X = line.Destination.X - line.Origin.X; Y = line.Destination.Y - line.Origin.Y}
let getStepFromDiff (diff: Coordinate) = if diff.X = 0 then {X = diff.X / (abs diff.Y); Y = diff.Y / (abs diff.Y) } else {X = diff.X / (abs diff.X); Y = diff.Y / (abs diff.X) }
let rec GetPointsBetween (start: Coordinate) (stop:Coordinate) (step: Coordinate) = if start = stop then Seq.singleton stop else seq {yield start; yield! (GetPointsBetween {X = start.X+step.X; Y = start.Y+step.Y} stop step)} 
let getAllPointsInLine (line : VentLine) : seq<Coordinate> = getDiff line |> fun diff -> GetPointsBetween line.Origin line.Destination (getStepFromDiff diff)

let d5p1 = parseAllVentLines |> filterOutDiagonals |> Seq.fold (fun acc l -> Seq.append acc (getAllPointsInLine l)) Seq.empty |> Seq.countBy (fun c -> c) |> Seq.filter (fun kvp -> snd kvp > 1) |> Seq.length
let d5p2 = parseAllVentLines |> Seq.fold (fun acc l -> Seq.append acc (getAllPointsInLine l)) Seq.empty |> Seq.countBy (fun c -> c) |> Seq.filter (fun kvp -> snd kvp > 1) |> Seq.length