module AOC2022.Day03

open System.IO

let input = File.ReadAllLines "data/input03.txt"

let items = input |> Array.map (fun s -> s.ToCharArray())

let intersection iss =
    Array.head iss
    |> Array.find (fun i -> Array.forall (Array.contains i) (Array.tail iss))

let priority =
    function
    | i when 'a' <= i && i <= 'z' -> int i - int 'a' + 1
    | i when 'A' <= i && i <= 'Z' -> int i - int 'A' + 27
    | _ -> failwith "invalid item"

let reduce = Array.sumBy (intersection >> priority)

// Part One
let sumOfSharedItemPriorities = items |> Array.map (Array.splitInto 2) |> reduce

// Part Two
let sumOfBadgeItemPriorities = items |> Array.chunkBySize 3 |> reduce
