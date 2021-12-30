module Solve20

open Input

type Image = {
    Mask: List<bool>
    Lights: Set<int*int>
    Borders: (int*int*int*int) //minx,miny,maxx,maxy
    OutOfBoundsDefault: bool
}

let parseMask line = Seq.map (fun c -> if c = '#' then true else false) line |> List.ofSeq
let maskIds (x,y) = [(x-1,y-1);(x,y-1);(x+1,y-1);(x-1,y);(x,y);(x+1,y);(x-1,y+1);(x,y+1);(x+1,y+1)]
let maskVals = [256;128;64;32;16;8;4;2;1]

let rec recParseImage mask maxx y (lines: list<string>) set = 
    match lines with 
    | [] -> {Mask = mask; Lights = set; Borders = (0,0,maxx,y-1); OutOfBoundsDefault = false}
    | h::t -> 
        let newLights = Seq.mapi(fun i c -> (i,c)) h |> Seq.choose (fun (x,c) -> if c = '#' then Some(x,y) else None) |> Set.ofSeq
        let newSet = Set.union set newLights
        recParseImage mask maxx (y+1) t newSet

let parseImage (lines: list<string>) = 
    let mask = parseMask lines[0]
    recParseImage mask (lines[2].Length - 1) 0 (List.skip 2 lines) Set.empty 
    
let getVal image coords =
    let (minx,miny,maxx,maxy) = image.Borders
    match coords with
    | (x,y) when x >= minx && x <= maxx && y>= miny && y<= maxy -> Set.contains coords image.Lights
    | _ -> image.OutOfBoundsDefault

let getNextVal image coords = 
    let index = maskIds coords |> List.map (getVal image) |> List.mapi (fun i v -> if v then maskVals[i] else 0) |> List.sum
    image.Mask[index]

let getNextImage image =
    let (minx,miny,maxx,maxy) = image.Borders
    let allCoordsInBorders = List.fold (fun acc y -> List.append acc (List.map (fun x -> (x,y)) [minx-1..maxx+1])) [] [miny-1..maxy+1]
    let newLights = List.choose (fun coord -> if (getNextVal image coord) then Some(coord) else None) allCoordsInBorders |> Set.ofList
    let newDefault = if image.OutOfBoundsDefault then image.Mask[511] else image.Mask[0]
    {Mask = image.Mask; Lights = newLights; Borders = (minx-1,miny-1,maxx+1,maxy+1); OutOfBoundsDefault = newDefault}

let rec imageAfterNIter n image = 
    match n with 
    | 0 -> image
    | _ -> imageAfterNIter (n-1) (getNextImage image)

let parseInput = readDayInput 20 |> List.ofSeq |> parseImage
let d20p1 = parseInput |> (imageAfterNIter 2) |> fun x -> x.Lights.Count
let d20p2 =  parseInput |> (imageAfterNIter 50) |> fun x -> x.Lights.Count