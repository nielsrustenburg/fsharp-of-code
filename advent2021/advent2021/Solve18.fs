module Solve18

open Input
open System
open FParsec

type SnailNumber =
    | Regular of int
    | Pair of SnailNumber * SnailNumber

type Direction =
    | L
    | R
    
module SnailNumber =
    let rec findExplosion (path: list<Direction>) (sn: SnailNumber) =
        let depth = path.Length
        match sn with
        | Regular(_) -> None
        | Pair(Regular(l),Regular(r)) when depth >= 4 -> Some((path,l,r))
        | Pair(l, r) -> Seq.tryPick id (seq{yield findExplosion (L::path) l; yield findExplosion (R::path) r})
    let rec leftExplodeStart path =
        match path with
        | R::t -> Some(List.rev (L::t)) // reverse because FindExplosion produces a reversed path
        | L::t -> leftExplodeStart t
        | [] -> None
    let rec rightExplodeStart path =
        match path with
        | L::t -> Some(List.rev (R::t)) // reverse because FindExplosion produces a reversed path
        | R::t -> rightExplodeStart t
        | [] -> None
    let rec findLeftMost sn initPath = 
        match (sn, initPath) with
        | (Pair(l,r), L::t) -> L::(findLeftMost l t) 
        | (Pair(l,r), R::t) -> R::(findLeftMost r t) 
        | (Pair(l,r), []) -> L::(findLeftMost l [])
        | (Regular(x), []) -> []
        | _ -> raise (Exception("this should not be happening"))
    let rec findRightMost sn initPath = 
        match (sn, initPath) with
        | (Pair(l,r), L::t) -> L::(findRightMost l t) 
        | (Pair(l,r), R::t) -> R::(findRightMost r t) 
        | (Pair(l,r), []) -> R::(findRightMost r [])
        | (Regular(x), []) -> []
        | _ -> raise (Exception("this should not be happening"))
    let rec explodeItAll revpath expl linstr rinstr sn =
        match (expl,linstr,rinstr,sn) with
        | (xpath,_,_,_) when xpath = revpath -> Regular(0)
        | (_,(Some(lpath),xl),_,Regular(n)) when lpath = revpath -> Regular(n+xl)
        | (_,_,(Some(rpath),xr),Regular(n)) when rpath = revpath -> Regular(n+xr)
        | (_,_,_,Regular(n)) -> Regular(n)
        | (_,_,_,Pair(l,r)) -> Pair(explodeItAll (L::revpath) expl linstr rinstr l , explodeItAll (R::revpath) expl linstr rinstr r)
    let findPath startF findF explPath sn = 
        let initPath = startF explPath
        match initPath with 
        | None -> None
        | Some(l) -> Some(List.rev (findF sn l)) // reverse it because we will build it reversed later..
    let findLeftPath explPath sn = findPath leftExplodeStart findRightMost explPath sn 
    let findRightPath explPath sn = findPath rightExplodeStart findLeftMost explPath sn 
    let tryExplode (sn:SnailNumber) = 
        let explosionLoc = findExplosion [] sn
        match explosionLoc with
        | None -> None
        | Some((p,l,r)) -> Some(explodeItAll [] p (findLeftPath p sn, l) (findRightPath p sn, r) sn)

    let rec findSplit (path: list<Direction>) (sn: SnailNumber) = 
        match sn with
        | Regular(x) when x > 9 -> Some(path)
        | Regular(_) -> None
        | Pair(l, r) -> Seq.tryPick id (seq {yield findSplit (L::path) l; yield findSplit (R::path) r})
    let rec splitIt path splitpath sn = 
        match sn with
        | Regular(n) when path = splitpath -> Pair(Regular(n/2),Regular((n/2)+(n%2)))
        | Regular(x) -> Regular(x)
        | Pair(l, r) -> Pair(splitIt (L::path) splitpath l, splitIt (R::path) splitpath r)

    let trySplit sn = 
        let splitPath = findSplit [] sn
        match splitPath with
        | None -> None
        | Some(p) -> Some(splitIt [] p sn)

    let tryReduceOnce sn =
        let exploded = tryExplode sn
        let expOrSpl = match exploded with
                        | Some(sn2) -> Some(sn2)
                        | None -> trySplit sn
        match expOrSpl with 
        | None -> (false, sn)
        | Some(sn2) -> (true, sn2)

    let reduce sn = 
        let rec recReduce (keepgoing,snail) =
            if keepgoing
            then recReduce (tryReduceOnce snail)
            else snail
        recReduce (true, sn)

    let add sna snb = 
        let newsn = Pair(sna,snb)
        reduce newsn

    let rec magnitude sn =
        match sn with
        | Regular(n) -> n
        | Pair(l,r) -> 3*(magnitude l) + 2*(magnitude r)
        

let snailValue, snailValueRef = createParserForwardedToRef<SnailNumber, unit>()

let psnail = pchar '[' >>. snailValue .>> pchar ',' .>>. snailValue .>> pchar ']' |>> Pair
let psnailRegular = pint32 |>> Regular
snailValueRef := choice[psnail;psnailRegular]

let unwrap x = match x with
                | Success(s,_,_) -> s
                | _ -> raise (Exception("waat"))

let parseInput sequence = 
    let parse1 s = run psnail s |> unwrap
    Seq.map parse1 sequence

let tryMoreMagnitude snl snr target =
    let mergesn = SnailNumber.add snl snr
    let result = SnailNumber.magnitude mergesn
    if result > target then result else target //maybe take this outside

let rec findBestMagnitude (snl,ml) (others) bestSoFar =
    match others with 
    | (snr, mr)::t when 3*ml + 2*mr > bestSoFar -> // Not sure if this is actually an upperbound, but it makes sense to me :D
        let result = tryMoreMagnitude snl snr bestSoFar  
        findBestMagnitude (snl,ml) t result
    | _ -> bestSoFar

let d18p1 = readDayInput 18 |> parseInput |> Seq.reduce SnailNumber.add |> SnailNumber.magnitude
let d18p2 = readDayInput 18 |> parseInput |> Seq.map (fun x -> x, SnailNumber.magnitude x) |> Seq.sortByDescending snd |> List.ofSeq |> fun l -> List.fold (fun acc x -> findBestMagnitude x (List.except [x] l) acc) 0 l 
