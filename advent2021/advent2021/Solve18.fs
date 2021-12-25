module Solve18

open Input
open System
open FParsec

type SnailNumber =
    | Regular of int
    | Pair of SnailNumber * SnailNumber
    
module SnailNumber =
    //let rec explode n d =
    //    match n with
    //    | Regular(x) -> (Regular(x),None,None,false)
    //    | Pair(Regular(l),Regular(r)) when d > 3 -> (Regular(0),Some(l),Some(r),true) // TODO: something smart
    //    | Pair(Regular(l),Pair(_,_)) -> 

    let split n = 
        match n with
        | Regular n when n > 9 -> Some(Pair(Regular(n/2),Regular((n/2)+(n%2))))
        | _ -> None

let ex = "[[1,2],[[3,4],5]]"
let exl = "[1,2]"
let exr = "[[3,4],5]"

let snailValue, snailValueRef = createParserForwardedToRef<SnailNumber, unit>()

let psnail = pchar '[' >>. snailValue .>> pchar ',' .>>. snailValue .>> pchar ']' |>> Pair
let psnailRegular = pint32 |>> Regular
snailValueRef := choice[psnail;psnailRegular]

let unwrap x = match x with
                | Success(s,_,_) -> s
                | _ -> raise (Exception("waat"))

let d18p1 = (run psnail ex, run psnail exl, run psnail exr) |> fun (a,b,c) -> (unwrap a,Pair(unwrap b, unwrap c)) |> fun (d,e) -> d = e 
let d18p2 = 0

