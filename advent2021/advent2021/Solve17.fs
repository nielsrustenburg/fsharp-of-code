module Solve17

open Input
open FParsec
open System
open Utils

let prange axis = pstring $"{axis}=" >>. pint32 .>> pstring ".." .>>. pint32 
let pinput = pstring "target area: " >>. prange "x" .>> pstring ", " .>>. prange "y"

let parseInput = 
    readDayInput 17 |> Seq.head |>
    run pinput |> 
    fun x -> match x with
             | Success(s,_,_) -> s
             | _ -> raise (Exception("waat"))
        
let resultBetween f min max x = 
    let result = f x
    result >= min && result <= max
let minXVelocity ((xmin, xmax),(ymin,ymax)) = Seq.find (resultBetween triangleNumber xmin xmax) (Seq.initInfinite id)
let maxYVelocity ((xmin, xmax),(ymin,ymax)) = triangleNumber ((abs(ymin))-1)
let minYVelocity ((xmin, xmax),(ymin,ymax)) = ymin
let maxXVelocity ((xmin, xmax),(ymin,ymax)) = xmax

let rec trajectoryHitsTarget ((xmin, xmax),(ymin,ymax)) (x,y) (vx, vy) =
    if resultBetween id xmin xmax x && resultBetween id ymin ymax y
    then true
    else 
        if x > xmax || y < ymin
        then false
        else 
            let vxd = match vx with
                      | v when v > 0 -> -1
                      | v when v < 0 -> +1
                      | _ -> 0
            trajectoryHitsTarget ((xmin,xmax),(ymin,ymax)) (x+vx, y+vy) ((vx+vxd),vy-1)

let allCandidates target = 
    let xvmin = minXVelocity target
    let xvmax = maxXVelocity target
    let xymin = minYVelocity target
    let xymax = maxYVelocity target
    List.fold (fun acc y -> List.append (List.map (fun x -> (x,y)) [xvmin..xvmax]) acc) [] [xymin..xymax]

let d17p1 = parseInput |> maxYVelocity
let d17p2 = parseInput |> fun target -> List.where (fun initVel -> trajectoryHitsTarget target (0,0) initVel) (allCandidates target) |> List.length