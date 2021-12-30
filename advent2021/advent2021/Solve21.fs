module Solve21

open Input
open FParsec // overkill :D
open System

let unwrap x = match x with
                | Success(s,_,_) -> s
                | _ -> raise (Exception("waat"))

let runwrap p s = run p s |> unwrap

let pstart = pstring "Player " >>. pint32 >>. pstring " starting position: " >>. pint32
let parseStartingPositions = readDayInput 21 |> List.ofSeq |> fun l -> (runwrap pstart l[0], runwrap pstart l[1])

let getDval1 n = (n%100)+1
let step1 pos n = ((pos + n + 9)%10)+1

let rec playUntilEnd1 (p1, p2, s1, s2, drolls, turn) = 
    let steps = getDval1 drolls + getDval1 (drolls+1) + getDval1 (drolls+2)
    match (s1 > 999, s2 > 999, turn) with
    | (true,_,_) -> s2*drolls
    | (_,true,_) -> s1*drolls
    | (_,_,true) ->
        let newpos = step1 p1 steps
        playUntilEnd1 (newpos,p2,s1+newpos,s2,drolls+3,false)
    | (_,_,false) -> 
        let newpos = step1 p2 steps
        playUntilEnd1 (p1,newpos,s1,s2+newpos,drolls+3,true)

let nextValidVals pos = 
    List.map (fun x -> ((pos-1+x)%10)+1) [3..9]

let memoization (f: 'a -> 'b ) = 
    let mutable (cache: Map<'a, 'b>) = Map.empty
    fun c -> 
        match cache.TryFind(c) with
        | Some(x) -> x
        | None -> 
            let result = f c
            cache <- cache.Add(c,result) // Not sure if this is efficient enough.. think this keeps making a new map..
            result
    

let rec generateValidSequences memNxtValVals (sequence:list<int>) (sum1,sum2) = // todo: there's room for caching p1,p2,sum1,sum2, but the start pos makes it more difficult than just going backwards for the entire thing from 21
    match (sum1 > 20, sum2 > 20, (sequence.Length % 2) = 0) with
    | (true,_,_) -> Seq.singleton (1,sequence)
    | (_,true,_) -> Seq.singleton (2,sequence)
    | (_,_,true) -> 
        let cpos = sequence[1]
        let (nvals: list<int>) = memNxtValVals cpos
        seq{yield! generateValidSequences memNxtValVals (nvals[0]::sequence) (sum1+nvals[0],sum2); 
            yield! generateValidSequences memNxtValVals (nvals[1]::sequence) (sum1+nvals[1],sum2);
            yield! generateValidSequences memNxtValVals (nvals[2]::sequence) (sum1+nvals[2],sum2);
            yield! generateValidSequences memNxtValVals (nvals[3]::sequence) (sum1+nvals[3],sum2);
            yield! generateValidSequences memNxtValVals (nvals[4]::sequence) (sum1+nvals[4],sum2);
            yield! generateValidSequences memNxtValVals (nvals[5]::sequence) (sum1+nvals[5],sum2);
            yield! generateValidSequences memNxtValVals (nvals[6]::sequence) (sum1+nvals[6],sum2);}
    | (_,_,false) -> 
        let cpos = sequence[1]
        let nvals = memNxtValVals cpos
        seq{yield! generateValidSequences memNxtValVals (nvals[0]::sequence) (sum1,sum2+nvals[0]); 
            yield! generateValidSequences memNxtValVals (nvals[1]::sequence) (sum1,sum2+nvals[1]);
            yield! generateValidSequences memNxtValVals (nvals[2]::sequence) (sum1,sum2+nvals[2]);
            yield! generateValidSequences memNxtValVals (nvals[3]::sequence) (sum1,sum2+nvals[3]);
            yield! generateValidSequences memNxtValVals (nvals[4]::sequence) (sum1,sum2+nvals[4]);
            yield! generateValidSequences memNxtValVals (nvals[5]::sequence) (sum1,sum2+nvals[5]);
            yield! generateValidSequences memNxtValVals (nvals[6]::sequence) (sum1,sum2+nvals[6]);}

let branchesPerVal n =
    match n with 
    | 3 -> int64(1)
    | 4 -> int64(3)
    | 5 -> int64(6)
    | 6 -> int64(7)
    | 7 -> int64(6)
    | 8 -> int64(3)
    | 9 -> int64(1)
    | _ -> raise (Exception("should not happen"))

let calcBranchAmount sequence = Seq.map2 (fun a2 a1 -> branchesPerVal(((a2+9-a1)%10)+1)) sequence (Seq.skip 2 sequence) |> Seq.reduce (fun acc x -> x*acc) 

let d21p1 = parseStartingPositions |> fun (p1,p2) -> playUntilEnd1 (p1,p2,0,0,0,true)
let d21p2 = 
    let sequences = parseStartingPositions |> fun (p1,p2) -> generateValidSequences (memoization nextValidVals) [p2;p1] (0,0) 
    let (wins1,wins2) = Seq.fold(fun (s1,s2) (winner,sequence) -> if winner = 1 then (s1+(calcBranchAmount sequence), s2) else (s1, s2+(calcBranchAmount sequence))) (int64(0),int64(0)) sequences
    if wins1 > wins2 then wins1 else wins2
