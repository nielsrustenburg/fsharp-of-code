module Solve2

open FParsec
open Input
open System

let xRock = pstring "A X"     .>> newline 
let xPaper = pstring "B X"    .>> newline
let xScissors = pstring "C X" .>> newline
let yRock = pstring "A Y"     .>> newline
let yPaper = pstring "B Y"    .>> newline
let yScissors = pstring "C Y" .>> newline
let zRock = pstring "A Z"     .>> newline
let zPaper = pstring "B Z"    .>> newline
let zScissors = pstring "C Z" .>> newline

let p11 = xPaper    >>% 1
let p12 = yScissors >>% 2
let p13 = zRock     >>% 3
let p14 = xRock     >>% 4
let p15 = yPaper    >>% 5
let p16 = zScissors >>% 6
let p17 = xScissors >>% 7
let p18 = yRock     >>% 8
let p19 = zPaper    >>% 9

let game1 = choice [
            p11
            p12
            p13
            p14
            p15
            p16
            p17
            p18
            p19
]

let d2p1 = readDayInputAsSingleString 2 |> runwrap (many game1) |> List.sum

let p21 = xPaper    >>% 1
let p22 = xScissors >>% 2
let p23 = xRock     >>% 3
let p24 = yRock     >>% 4
let p25 = yPaper    >>% 5
let p26 = yScissors >>% 6
let p27 = zScissors >>% 7
let p28 = zRock     >>% 8
let p29 = zPaper    >>% 9

let game2 = choice [
    p21
    p22
    p23
    p24
    p25
    p26
    p27
    p28
    p29
]

let d2p2 = readDayInputAsSingleString 2 |> runwrap (many game2) |> List.sum