module Solve13
open FParsec
open Input

type Dot = {
    X: int
    Y:int
}

type FoldLine = {
    Axis: char
    Position: int
}

let pdot = pint32 .>> pchar ',' .>>. pint32 |>> fun (x,y) -> {X = x; Y = y} 
let pfold = pstring "fold along " >>. anyChar .>> pchar '=' .>>. pint32 |>> fun (axis, pos) -> {Axis = axis; Position = pos}

let filterForSuccess parsed = 
    match parsed with 
    | Success(result, _, _)   -> Some(result)
    | Failure(errorMsg, _, _) -> None

let parseInput = 
    readDayInput 13 |> Seq.map (fun s -> (run pdot s, run pfold s)) 
    |> fun seq -> (Seq.choose filterForSuccess (Seq.map fst seq), Seq.choose filterForSuccess (Seq.map snd seq))
    |> fun (dots, folds) -> (Set.ofSeq dots, List.ofSeq folds)

let (|Same|FoldX|FoldY|) ((dot:Dot), (fold:FoldLine)) =
    if fold.Axis = 'x' && dot.X > fold.Position
    then 
        FoldX
    else
        if fold.Axis = 'y' && dot.Y > fold.Position
        then FoldY
        else Same

let foldDot (fold:FoldLine) (dot:Dot) = 
    match (dot,fold) with
    | FoldX -> {X = (2*fold.Position)-dot.X; Y = dot.Y}
    | FoldY -> {X = dot.X; Y = (2*fold.Position)-dot.Y}
    | Same -> dot

let foldPaper (fold:FoldLine) (dots:Set<Dot>) = Set.map (fun x -> foldDot fold x) dots

let dotsToString slength y dots = 
    let charFunc x = if Set.exists (fun d -> d.X = x && d.Y = y) dots then '#' else '.'
    Seq.map (charFunc) {0 .. slength} |> Seq.toArray |> System.String

let visualDots (dots:Set<Dot>) = 
    let maxX = (Set.toList dots |> List.maxBy (fun d -> d.X)).X
    let maxY = (Set.toList dots |> List.maxBy (fun d -> d.Y)).Y
    Seq.map (fun y -> dotsToString maxX y dots) {0 .. maxY}

let d13p1 = parseInput |> fun (dots, folds) -> foldPaper folds[0] dots |> Set.count
let d13p2 = parseInput |> fun (dots, folds) -> Seq.fold (fun remDots fold -> foldPaper fold remDots) dots folds |> visualDots // Folding folds :D