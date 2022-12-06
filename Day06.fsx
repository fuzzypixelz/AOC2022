#load "Common.fsx"

open System.IO
open Common

let input = File.ReadAllText "data/input06.txt"

let marker variant =
    Seq.toArray
    >> Array.windowed variant
    >> Array.findIndex (Array.distinct >> Array.length >> (=) variant)
    >> (+) variant

partOne
    "How many characters need to be processed before the first start-of-packet marker is detected?"
    (input |> marker 4)

partTwo
    "How many characters need to be processed before the first start-of-message marker is detected?"
    (input |> marker 14)
