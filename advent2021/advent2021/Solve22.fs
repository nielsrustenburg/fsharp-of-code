module Solve22

open Input
open FParsec
open System

let ex = "on x=-31..15,y=-40..12,z=-21..27"

let pstate = pstring "on " <|> pstring "off " |>> fun x -> if x = "on " then true else false
let prange = pint32 .>> pstring ".." .>>. pint32
let pcube = tuple3 (pstring "x=" >>. prange) (pstring ",y=" >>. prange) (pstring ",z=" >>. prange)
let pinstruction = pstate .>>. pcube

let unwrap x = match x with
                | Success(s,_,_) -> s
                | _ -> raise (Exception("waat"))

let runwrap p s = run p s |> unwrap

let noValAbove50 (min,max) = min > -50 && max < 50
let isInitCube (_, (x,y,z)) = noValAbove50 x && noValAbove50 y && noValAbove50 z

let parseInputs = readDayInput 22 |> List.ofSeq |> List.map (runwrap pinstruction)

let generateAllCoords2D ((minx,maxx),(miny,maxy)) = 
    let helper x (min, max) = Seq.map (fun a -> (x,a)) (seq{min..max})
    Seq.fold (fun acc b -> seq{yield! acc; yield! (helper b (miny,maxy));}) Seq.empty (seq{minx..maxx}) 
let generateAllCoords3D ((minx,maxx),(miny,maxy),(minz,maxz)) =  
    let pairSeqs = generateAllCoords2D ((minx,maxx),(miny,maxy)) |> List.ofSeq
    let helper z pseqs = Seq.map (fun (x,y) -> (x,y,z)) pseqs
    Seq.fold (fun acc z -> seq{yield! acc; yield! helper z pairSeqs}) Seq.empty (seq{minz..maxz}) |> Set.ofSeq
let rec solveSmall inputs onSet =
    match inputs with
    | [] -> onSet
    | (true,xyz)::t -> 
        let newOnset = Set.union onSet (generateAllCoords3D xyz)
        solveSmall t newOnset
    | (false,xyz)::t ->
        let newOnset = Set.difference onSet (generateAllCoords3D xyz)
        solveSmall t newOnset
    
let lineIntersect (lineA: int*int) (lineB: int*int) =
    match (lineA, lineB) with
    | ((aStart, aEnd),(bStart,bEnd)) when aStart > bEnd || bStart > aEnd -> None
    | ((aStart, aEnd),(bStart,bEnd)) when aStart < bStart && bEnd < aEnd -> Some(bStart,bEnd)
    | ((aStart, aEnd),(bStart,bEnd)) when aStart < bStart && bEnd >= aEnd -> Some(bStart,aEnd)
    | ((aStart, aEnd),(bStart,bEnd)) when aStart >= bStart && bEnd < aEnd -> Some(aStart,bEnd)
    | ((aStart, aEnd),(bStart,bEnd)) when aStart >= bStart && bEnd >= aEnd -> Some(aStart,aEnd)
    | _ -> raise (Exception("hmm? incomplete match?"))

let intersect (ax,ay,az) (bx,by,bz) = 
    let isectOptions = (lineIntersect ax bx, lineIntersect ay by, lineIntersect az bz)
    match isectOptions with
    | (Some(x),Some(y),Some(z)) -> Some((x,y,z))
    | _ -> None

let intersectMany cubes = 
    let helper optA optB = 
       match (optA, optB) with
       | (Some(a),Some(b)) -> intersect a b
       | _ -> None
    List.reduce (fun acc cube -> helper acc cube) (List.map (fun c -> Some(c)) cubes)


let d22p1 = parseInputs |> List.takeWhile isInitCube |> fun smallInputs -> solveSmall smallInputs Set.empty |> Set.count
let d22p2 = runwrap pinstruction ex