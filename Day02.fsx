open System.IO

let input = File.ReadAllLines "data/input02.txt"

type Move =
    | Rock
    | Paper
    | Scissors

type Round = { Player: Move; Opponent: Move }

let toMove =
    function
    | 'A'
    | 'X' -> Rock
    | 'B'
    | 'Y' -> Paper
    | 'C'
    | 'Z' -> Scissors
    | _ -> failwith "invalid move"

let toRound (str: string) =
    { Player = toMove str[2]
      Opponent = toMove str[0] }

let roundScore round =
    let shapeScore =
        match round.Player with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let outcomeScore =
        match (round.Player, round.Opponent) with
        // Win
        | Rock, Scissors
        | Scissors, Paper
        | Paper, Rock -> 6
        // Draw
        | p, o when p = o -> 3
        // Loss
        | _ -> 0

    shapeScore + outcomeScore

// Part One
let totalScore = input |> Array.sumBy (toRound >> roundScore)

type Outcome =
    | Win
    | Loss
    | Draw

type Round' = { Result: Outcome; Opponent: Move }

let toOutcome =
    function
    | 'X' -> Loss
    | 'Y' -> Draw
    | 'Z' -> Win
    | _ -> failwith "invalid outcome"

let toRound' (str: string) =
    { Result = toOutcome str[2]
      Opponent = toMove str[0] }

// Reduction of the second problem to the first
let inferRound round' =
    let move =
        match round'.Result with
        | Win ->
            match round'.Opponent with
            | Rock -> Paper
            | Paper -> Scissors
            | Scissors -> Rock
        | Loss ->
            match round'.Opponent with
            | Rock -> Scissors
            | Paper -> Rock
            | Scissors -> Paper
        | Draw -> round'.Opponent

    { Player = move
      Opponent = round'.Opponent }

// Part Two
let revisedTotalScore = input |> Array.sumBy (toRound' >> inferRound >> roundScore)
