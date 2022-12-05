#load "Common.fsx"

open System
open Common
open FParsec

type Crate = char
type Stack = int
type Cargo = List<Crate>[] // map Stack to List<Crate>
type Instr = { Count: int; From: Stack; To: Stack }
type Crane = Cargo * List<Instr> -> Cargo

let spc = pchar ' '

let crate: Parser<Option<Crate>> =
    choice [ between (pstring "[") (pstring "]") upper |>> Some; pstring "   " >>% None ]

let cargo: Parser<Cargo> =
    many1 (sepBy1 crate spc .>> newline)
    |>> List.transpose
    |>> List.map (List.choose id)
    |>> Array.ofList

let labels: Parser<unit> = skipManyTill anyChar newline .>> newline // useless!

let instrs: Parser<List<Instr>> =
    let chunk s = pstring s >>. spc >>. puint64 |>> int

    let make count from to_ =
        { Count = count; From = from; To = to_ }

    let instr =
        pipe3
            (chunk "move" .>> spc)
            // y u start at 1?
            (chunk "from" |>> (+) -1 .>> spc)
            (chunk "to" |>> (+) -1 .>> newline)
            make

    many1 instr

let procedure: Parser<Cargo * List<Instr>> = cargo .>> labels .>>. instrs .>> eof

let crane variant procedure =
    let oneStep (cargo: Cargo) { Count = c; From = f; To = t } =
        Array.mapi
        <| function
            | i when i = f -> List.skip c
            | i when i = t -> List.append (List.take c cargo[f] |> variant)
            | _ -> id
        <| cargo

    procedure ||> List.fold oneStep

let crane9000: Crane = crane List.rev
let crane9001: Crane = crane id

let reduce: Cargo -> string = Array.map List.head >> String.Concat

let input = runParserOnFile' procedure "data/input05.txt"

crane9000 input
|> reduce
|> partOne "After the rearrangement procedure completes, what crate ends up on top of each stack?"

crane9001 input
|> reduce
|> partTwo "After the rearrangement procedure completes, what crate ends up on top of each stack?"
