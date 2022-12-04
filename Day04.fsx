#load "Common.fsx"
open Common

#r "nuget: FParsec"
open FParsec

type Assignment(start: uint32, end_: uint32) =
    member self.Start = start
    member self.End = end_

    static member SubsetOf(self: Assignment, other: Assignment) =
        self.Start <= other.Start && other.End <= self.End

    static member Contained(self: Assignment, other: Assignment) =
        Assignment.SubsetOf(self, other) || Assignment.SubsetOf(other, self)

    member self.Contains(section: uint32) =
        self.Start <= section && section <= self.End

    static member Overlapping(self: Assignment, other: Assignment) =
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

let sections =
    runParserOnFile (many (pair .>> newline)) () "data/input04.txt" (System.Text.ASCIIEncoding())
    |> function
        | Success(i, _, _) -> Array.ofList i
        | Failure(e, _, _) -> failwith e

Array.filter Assignment.Contained sections
|> Array.length
|> partOne "In how many assignment pairs does one range fully contain the other?"

Array.filter Assignment.Overlapping sections
|> Array.length
|> partTwo "In how many assignment pairs do the ranges overlap?"
