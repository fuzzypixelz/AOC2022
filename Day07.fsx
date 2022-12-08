#load "Common.fsx"

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Common

type File = { Name: string; Size: int }

type Directory =
    { Name: string
      Parent: Directory
      Files: List<File>
      Subdirectories: List<Directory> }

    static member Fold folder state dir =
        let state' = folder state dir
        Seq.fold (Directory.Fold folder) state' dir.Subdirectories

    static member Size dir =
        Seq.sumBy (fun f -> f.Size) dir.Files
        + Seq.sumBy Directory.Size dir.Subdirectories

    static member FindAllSizes p =
        Directory.Fold
            (fun sizes dir -> let size = Directory.Size dir in if p size then size :: sizes else sizes)
            List.empty

let input =
    let rec root =
        { Name = "/"
          Parent = root
          Files = List()
          Subdirectories = List() } in

    let mutable fs = root

    let ls = Regex @"^\$ ls$"
    let cd = Regex @"^\$ cd (\w+|\/|\.+)$"

    let mkdir = Regex @"^dir (.+)$"
    let touch = Regex @"^(\d+) (.+)$"

    for line in IO.File.ReadLines "data/input07.txt" do
        if ls.IsMatch line then
            ()
        elif cd.IsMatch line then
            fs <-
                match cd.Match(line).Groups[1].Value with
                | "/" -> root
                | ".." -> fs.Parent
                | name -> fs.Subdirectories.Find(fun dir -> dir.Name = name)
        elif mkdir.IsMatch line then
            fs.Subdirectories.Add
                { Name = mkdir.Match(line).Groups[1].Value
                  Parent = fs
                  Files = List()
                  Subdirectories = List() }
        elif touch.IsMatch line then
            fs.Files.Add
                { Name = touch.Match(line).Groups[2].Value
                  Size = int touch.Match(line).Groups[1].Value }
        else
            failwith $"invalid input: {line}"

    root

input
|> Directory.FindAllSizes(fun size -> size <= 100_000)
|> List.sum
|> partOne "What is the sum of the total sizes of those directories?"

let neededSize = 30_000_000
let unusedSize = 70_000_000 - Directory.Size input

input
|> Directory.FindAllSizes(fun size -> size + unusedSize >= neededSize)
|> List.sort
|> List.head
|> partTwo
    "Find the smallest directory that, if deleted, \
    would free up enough space on the filesystem to run the update. \
    What is the total size of that directory?"
