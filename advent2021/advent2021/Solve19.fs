module Solve19

open Input
open FParsec
open System
open Utils
// Find beacon-relative distances
// Try for all 24 rotations (or until you find one where 12+ beacons match)

type Rotation = 
    | A0 = 0
    | A90 = 1
    | A180 = 2
    | A270 = 3

let zeroAngle = (Rotation.A0,Rotation.A0,Rotation.A0)

type Coord = {
    X: int
    Y: int
    Z: int
}
let zeroCoord = {X=0;Y=0;Z=0}
        
type BeaconSet = {
    ScannerId: int
    Offset: Coord
    Angle: (Rotation*Rotation*Rotation)
    Beacons: List<Coord>
}

let unwrap x = match x with
                | Success(s,_,_) -> s
                | _ -> raise (Exception("waat"))

let runwrap p s = run p s |> unwrap

let pscanner = pstring "--- scanner " >>. pint32 .>> pstring " ---" .>> spaces
let pbeacon = tuple3 (pint32 .>> pchar ',') (pint32 .>> pchar ',') (pint32) .>> spaces |>> fun (x,y,z) -> {X = x;Y = y;Z = z}
let pbeaconSet = pscanner .>>. (many (pbeacon)) |>> fun (scanid, beacons) -> {ScannerId = scanid; Offset = zeroCoord; Angle = zeroAngle; Beacons = beacons}
let pmanyBeaconSets = many pbeaconSet

let rec rotate rotation (a,b) = 
    match rotation with 
    | Rotation.A0 -> (a,b)
    | Rotation.A90 -> (-b,a)
    | Rotation.A180 -> (-a,-b)
    | Rotation.A270 -> (b,-a)
    | x -> rotate (x % enum<Rotation>(4)) (a,b)
let allRotations = seq {Rotation.A0;Rotation.A90;Rotation.A180;Rotation.A270}

let rotateX rotation (x,y,z) = 
    let (newy, newz) = rotate rotation (y,z)
    (x,newy,newz)

let rotateY rotation (x,y,z) = 
    let (newz,newx) = rotate rotation (z,x)
    (newx,y,newz)

let rotateZ rotation (x,y,z) = 
    let (newx,newy) = rotate rotation (x,y)
    (newx,newy,z)

let rotateAngle (rotX,rotY,rotZ) (coord:Coord) =
    let (x,y,z) = (coord.X,coord.Y,coord.Z)
    let (x,y,z) = rotateX rotX (x,y,z)
    let (x,y,z) = rotateY rotY (x,y,z)
    let (x,y,z) = rotateZ rotZ (x,y,z)
    {X=x;Y=y;Z=z}

let asRotatedBy (rotX,rotY,rotZ) (bset:BeaconSet) =
    let (brotX, brotY, brotZ) = bset.Angle
    let newAngle = (enum<Rotation>(int(rotX)+int(brotX)%4), enum<Rotation>(int(rotY)+int(brotY)%4), enum<Rotation>(int(rotZ)+int(brotZ)%4))
    {
        ScannerId = bset.ScannerId;
        Offset = rotateAngle (rotX,rotY,rotZ) bset.Offset
        Angle = newAngle
        Beacons = List.map (rotateAngle (rotX,rotY,rotZ)) bset.Beacons
    }

let allAngles = 
    let l1 = Seq.repeat (4*4) allRotations
    let l2 = Seq.repeat (4) (Seq.repeatElements 4 allRotations)
    let l3 = Seq.repeatElements (4*4) allRotations
    Seq.map3 (fun (x:Rotation) (y:Rotation) (z:Rotation) -> (x,y,z)) l1 l2 l3

let offsetCoord (x,y,z) coord = {X=coord.X+x;Y=coord.Y+y;Z=coord.Z+z}
let dist coord = sqrt (double(coord.X*coord.X + coord.Y*coord.Y + coord.Z*coord.Z))
let asOffsetFromNeg (offset:Coord) (bset:BeaconSet) = // At this point not sure if this even should be negative.. Don't use unless know what you're doing
    {
        ScannerId = bset.ScannerId;
        Offset = offset;
        Angle = bset.Angle;
        Beacons = List.map (fun x -> offsetCoord (-offset.X, -offset.Y, -offset.Z) x) bset.Beacons
    }

let applyOffset (offset:Coord) (bset:BeaconSet) =
    {
        ScannerId = bset.ScannerId;
        Offset = offset;
        Angle = bset.Angle;
        Beacons = List.map (fun x -> offsetCoord (offset.X, offset.Y, offset.Z) x) bset.Beacons
    }

let haveTwelvePlusMatches list1 list2 = List.fold (fun (acccount,acclist) x -> if List.contains x acclist then (acccount+1, List.except [x] acclist) else (acccount,acclist)) (0,list1) list2 |> fun (count, _) -> count > 11 

let tryMatch bset1 bset2 =
    let offsetAndDists set x = asOffsetFromNeg x set |> fun offsetBset -> (offsetBset, List.map dist offsetBset.Beacons)
    let b1offsetsAndDists = List.map (fun x -> offsetAndDists bset1 x) bset1.Beacons
    let b2offsetsAndDists = List.map (fun x -> offsetAndDists bset2 x) bset2.Beacons
    let pairs = Seq.fold (fun (acc) x -> List.append acc (List.map (fun z -> (x,z)) b2offsetsAndDists)) [] b1offsetsAndDists
    let bmatch = List.tryFind (fun ((_,d1),(_,d2)) -> haveTwelvePlusMatches d1 d2) pairs
    match bmatch with
    | None -> None
    | Some((b1,_),(b2,_)) -> 
        let angle = Seq.tryFind (fun x -> haveTwelvePlusMatches b1.Beacons (List.map (rotateAngle x) b2.Beacons)) allAngles
        match angle with
        | Some(a) -> 
            let offsAfterAngle2 = rotateAngle a b2.Offset
            Some((a,{X=b1.Offset.X - offsAfterAngle2.X + (2*bset1.Offset.X);Y=b1.Offset.Y - offsAfterAngle2.Y + (2*bset1.Offset.Y);Z=b1.Offset.Z - offsAfterAngle2.Z + (2*bset1.Offset.Z)}))
        | None -> None

let findMatch bset1 otherSets = 
    let addScannerId sid x = 
        match x with
        | Some(y) -> Some(sid, y)
        | None -> None
    List.tryPick (fun x -> addScannerId x.ScannerId (tryMatch bset1 x)) otherSets 

let findMatches bset1 otherSets = 
    let addScannerId sid x = 
        match x with
        | Some(y) -> Some(sid, y)
        | None -> None
    List.map (fun x -> addScannerId x.ScannerId (tryMatch bset1 x)) otherSets |>
    List.choose id

let rec findRemainingOrderedScanners bset1 otherSets =
    match otherSets with
    | [] -> []
    | notempty ->
        let result = findMatch bset1 notempty
        match result with 
        | None -> raise (Exception("Something's wrong with input or algorithm"))
        | Some((scannerId, (angle,offset))) -> 
            let bset2 = List.find (fun x -> x.ScannerId = scannerId) notempty
            let relativeBset2 = asOffsetFromNeg offset (asRotatedBy angle bset2)
            let remainingSets = List.where (fun x -> not (x.ScannerId = scannerId)) notempty
            bset1::findRemainingOrderedScanners relativeBset2 remainingSets

let rec findRemainingOrderedScanners2 acc otherSets tried =
    let getScannerResult (relAngle, relOffset) (scannerId, (angle,offset)) =
        let bset2 = List.find (fun x -> x.ScannerId = scannerId) otherSets
        let rotated = (asRotatedBy angle bset2)
        let relativeOffset = {X=offset.X - (2*relOffset.X); Y=offset.Y - (2*relOffset.Y); Z= offset.Z - (2*relOffset.Z)}
        applyOffset relativeOffset rotated
    match otherSets with
    | [] -> acc
    | notempty ->
        let scannable = List.where(fun x -> not (List.contains x.ScannerId tried)) acc
        match scannable with
        | [] -> raise (Exception("Something's wrong with input or algorithm"))
        | h::t -> 
            let results = findMatches h otherSets
            let resultIds = List.map (fun (sid,_) -> sid) results
            let remainingSets = List.where (fun x -> not (List.contains x.ScannerId resultIds)) otherSets 
            let newAcc = List.append acc (List.map (getScannerResult (h.Angle, h.Offset)) results)
            findRemainingOrderedScanners2 newAcc remainingSets (h.ScannerId::tried)



let findOrderedScanners bsets = 
    let scannerzero = List.find (fun x -> x.ScannerId = 0) bsets
    let rest = List.where (fun x -> not(x.ScannerId = 0)) bsets
    findRemainingOrderedScanners scannerzero rest

let findOrderedScanners2 bsets = 
    let scannerzero = List.find (fun x -> x.ScannerId = 0) bsets
    let rest = List.where (fun x -> not(x.ScannerId = 0)) bsets
    findRemainingOrderedScanners2 [scannerzero] rest []

let findUniqueBeaconSet allbeaconsets = 
    let asSets = List.map (fun bs -> Set.ofList bs.Beacons) allbeaconsets
    List.reduce (fun acc x -> Set.union acc x) asSets

let manhatDist c1 c2 = abs (c1.X - c2.X) + abs (c1.Y - c2.Y) + abs (c1.Z - c2.Z)
let bestManHatDistForElement c1 lst = List.map (fun x -> manhatDist c1 x) lst |> List.max
let bestManHatDist lst = List.map (fun x -> bestManHatDistForElement x lst) lst |> List.max 

let d19p1 = readDayInputAsSingleString 19 |> runwrap pmanyBeaconSets |> findOrderedScanners2 |> findUniqueBeaconSet |> Set.count
let d19p2 = readDayInputAsSingleString 19 |> runwrap pmanyBeaconSets |> findOrderedScanners2 |> List.map (fun x -> x.Offset) |> bestManHatDist


