open System.IO

let input = File.ReadAllText "data/input01.txt"
let inventories = input.Split "\n\n"

let totalCalories (inventory: string) =
    // NOTE: There is an unwanted trailing newline
    let inventory = inventory.TrimEnd()
    inventory.Split "\n" |> Seq.sumBy int

let calories = inventories |> Array.map totalCalories

// Part One
let topElf = calories |> Array.max

// Part Two
let topThreeElves = calories |> Array.sortDescending |> Array.take 3 |> Array.sum
