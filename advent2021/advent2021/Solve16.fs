module Solve16

open Input
open FParsec
open System

type Header = {
    Version: int
    TypeId: int
}

let versionToOperatorFunc v : list<int64> -> int64 =
    match v with
    | 0 -> List.sum
    | 1 -> List.reduce (fun acc x -> acc*x)
    | 2 -> List.min
    | 3 -> List.max
    | 5 -> fun (l) -> if l[0] > l[1] then 1 else 0
    | 6 -> fun (l) -> if l[0] < l[1] then 1 else 0
    | 7 -> fun (l) -> if l[0] = l[1] then 1 else 0
    | x -> raise (Exception($"not expected {x}"))

type IPacket =
    abstract member Version : int
    abstract member TypeId : int
    abstract member Value : int64
    abstract member SumVersion : int

type LiteralPacket(header: Header, value: int64) = 
    interface IPacket with
        member this.Version = header.Version
        member this.TypeId = header.TypeId
        member this.Value = value
        member this.SumVersion = header.Version

type OperatorPacket(header: Header, packets: list<IPacket>) = 
    interface IPacket with
        member this.Version = header.Version
        member this.TypeId = header.TypeId
        member this.Value = (versionToOperatorFunc header.TypeId) (List.map (fun (x:IPacket) -> x.Value) packets)
        member this.SumVersion = (List.sumBy (fun (x:IPacket) -> x.SumVersion) packets) + header.Version

let rec exactlyN p n = 
    match n with 
    | 0 -> preturn []
    | _  when n > 0 -> pipe2 p (exactlyN p (n-1)) (fun a b -> (a::b))
    | _ -> pzero

let unwrapParsed parsed = 
    match parsed with 
    | Success(s,_,_) -> s
    | _ -> raise (Exception("waat"))

let phexcto4bin = anyString 1 |>> fun x -> Convert.ToInt32(x,16) |> fun y -> Convert.ToString(y,2).PadLeft(4,'0')
let phexStrToBin = (many phexcto4bin) |>> String.concat ""
let readInputToBinary = 
    readDayInput 16 |> 
    Seq.head |> 
    run phexStrToBin |> unwrapParsed

let pheader = anyString 3 .>>. anyString 3 |>> fun (v,t) -> { Version = Convert.ToInt32(v,2); TypeId = Convert.ToInt32(t,2) }

let partialLiteral = pchar '1' >>. anyString 4
let lastLiteral = pchar '0' >>. anyString 4
let pliteral = many partialLiteral .>>. lastLiteral |>> fun (manys, s) -> String.concat "" (seq {yield! manys; yield s}) |> fun x -> Convert.ToInt64(x,2)

let pFailIfNotLiteralHeader header = 
    match header.TypeId with
    | 4 -> pliteral |>> fun x -> (header,x)
    | _ -> pzero

let pliteralpacket = pheader >>=? pFailIfNotLiteralHeader |>> fun (header,value) -> LiteralPacket(header,value) |> fun x -> (x:>IPacket)

let packetValue, packetValueRef = createParserForwardedToRef<IPacket, unit>()
let pManyPackets = many packetValue

let psubPacket0Length = pchar '0' >>. anyString 15 |>> fun l -> Convert.ToInt32(l,2)
let psubPacket0 = pheader .>>.? (psubPacket0Length >>= (fun n -> anyString n)) |>> fun (h,s) -> OperatorPacket(h, unwrapParsed (run pManyPackets s))
let psubPacket1Length = pchar '1' >>. anyString 11 |>> fun l -> Convert.ToInt32(l,2)
let psubPacket1 = pheader .>>.? (psubPacket1Length >>= (fun n -> exactlyN packetValue n)) |>> fun (h,ps) -> OperatorPacket(h, ps)

let poperatorpacket = psubPacket0 <|> psubPacket1 |>> fun x -> (x:>IPacket)

packetValueRef := pliteralpacket <|> poperatorpacket

let getSumVersion (p: IPacket) = p.SumVersion 
let getValue (p: IPacket) = p.Value
let d16p1 = readInputToBinary |> run packetValue |> unwrapParsed |> getSumVersion
let d16p2 = readInputToBinary |> run packetValue |> unwrapParsed |> getValue