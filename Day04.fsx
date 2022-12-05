#load "Common.fsx"
open Common

#r "nuget: FParsec"
open FParsec

type Assignment(start: uint32, end_: uint32) =
    member self.Start = start
    member self.End = end_

    member self.Contains(section: uint32) =
        self.Start <= section && section <= self.End

    member self.Contains(other: Assignment) =
        self.Start <= other.Start && other.End <= self.End

    static member AreFullyOverlapping(self: Assignment, other: Assignment) =
        self.Contains(other) || other.Contains(self)

    static member AreOverlapping(self: Assignment, other: Assignment) =
        (*
            [      [----]            ]
            s      s'   e            e'
        or
            [      [----]            ]
            s'     s    e'           e
        *)
        self.Contains(other.Start) || other.Contains(self.Start)

let assignment: Parser<Assignment, unit> =
    tuple2 (puint32 .>> pstring "-") puint32 |>> Assignment

let pair: Parser<Assignment * Assignment, unit> =
    tuple2 (assignment .>> pstring ",") assignment

let input =
    runParserOnFile' (many (pair .>> newline) |>> Array.ofList) "data/input04.txt"

Array.filter Assignment.AreFullyOverlapping input
|> Array.length
|> partOne "In how many assignment pairs does one range fully contain the other?"

Array.filter Assignment.AreOverlapping input
|> Array.length
|> partTwo "In how many assignment pairs do the ranges overlap?"
