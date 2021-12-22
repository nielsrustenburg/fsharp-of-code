﻿module Utils
open System

let charToInt (c:char) = int c - int '0'

module Array2D =
    let filterCoords f (s:'a[,]) = Seq.map (fun x -> Seq.map (fun y -> (x,y)) {0..(Array2D.length2 s) - 1}) {0..(Array2D.length1 s) - 1} |>
                                    Seq.concat |> Seq.filter (fun (x,y) -> (f s x y))

    let getNeighbours4 arr x y = 
        match (x > 0, y > 0, Array2D.length1 arr - x > 1, Array2D.length2 arr - y > 1) with
        | (true, true, true, true) -> [|(x-1,y);(x,y-1);(x+1,y);(x,y+1)|]
        | (false, true, true, true) -> [|(x,y-1);(x+1,y);(x,y+1)|]
        | (true, false, true, true) -> [|(x-1,y);(x+1,y);(x,y+1)|]
        | (true, true, false, true) -> [|(x-1,y);(x,y-1);(x,y+1)|]
        | (true, true, true, false) -> [|(x-1,y);(x,y-1);(x+1,y)|]
        | (false, false, true, true) -> [|(x+1,y);(x,y+1)|]
        | (true, false, false, true) -> [|(x-1,y);(x,y+1)|]
        | (true, true, false, false) -> [|(x-1,y);(x,y-1)|]
        | (false, true, true, false) -> [|(x,y-1);(x+1,y)|]
        | _ -> raise (Exception("WHAT IS GOING ON?!?!?"))

    let getNeighbours8 arr x y = 
        match (x > 0, y > 0, Array2D.length1 arr - x > 1, Array2D.length2 arr - y > 1) with
        | (true, true, true, true) -> [|(x-1,y);(x,y-1);(x+1,y);(x,y+1);(x-1,y-1);(x+1,y-1);(x+1,y+1);(x-1,y+1)|]
        | (false, true, true, true) -> [|(x,y-1);(x+1,y);(x,y+1);(x+1,y-1);(x+1,y+1)|]
        | (true, false, true, true) -> [|(x-1,y);(x+1,y);(x,y+1);(x+1,y+1);(x-1,y+1)|]
        | (true, true, false, true) -> [|(x-1,y);(x,y-1);(x,y+1);(x-1,y-1);(x-1,y+1)|]
        | (true, true, true, false) -> [|(x-1,y);(x,y-1);(x+1,y);(x-1,y-1);(x+1,y-1)|]
        | (false, false, true, true) -> [|(x+1,y);(x,y+1);(x+1,y+1)|]
        | (true, false, false, true) -> [|(x-1,y);(x,y+1);(x-1,y+1)|]
        | (true, true, false, false) -> [|(x-1,y);(x,y-1);(x-1,y-1)|]
        | (false, true, true, false) -> [|(x,y-1);(x+1,y);(x+1,y-1)|]
        | _ -> raise (Exception("WHAT IS GOING ON?!?!?"))

    let getNeighbour4Set arr x y = getNeighbours4 arr x y |> Set.ofArray
    let getNeighbour8Set arr x y = getNeighbours8 arr x y |> Set.ofArray