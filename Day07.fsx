#load "Common.fsx"

open Common
open FParsec
open System.Collections.Generic

// let tr x =
//     printfn "@@@ %A" x
//     x

let sample =
    [ "$ cd /"
      "$ ls"
      "dir a"
      "14848514 b.txt"
      "8504156 c.dat"
      "dir d"
      "$ cd a"
      "$ ls"
      "dir e"
      "29116 f"
      "2557 g"
      "62596 h.lst"
      "$ cd e"
      "$ ls"
      "584 i"
      "$ cd .."
      "$ cd .."
      "$ cd d"
      "$ ls"
      "4060174 j"
      "8033020 d.log"
      "5626152 d.ext"
      "7214296 k" ]
    |> String.concat "\n"

type Filesystem =
    | File of size: uint64 * name: string
    | Dir of name: string * parent: Option<Filesystem> * children: List<Filesystem>

let getName =
    function
    | File(_, n) -> n
    | Dir(n, _, _) -> n

let parent =
    function
    | File _ -> None
    | Dir(_, p, _) -> p

let getChild name =
    function
    | File _ -> None
    | Dir(_, _, cs) -> Some(cs.Find(getName >> (=) name))

let addChild fs' fs =
    match fs with
    | File _ -> ()
    | Dir(_, _, cs) -> cs.Add(fs')

    fs

let addDir name fs =
    addChild (Dir(name, Some(fs), List())) fs

let root =
    function
    | Dir("/", _, _) as root -> root
    | dir -> parent dir |> Option.get

type Parser = Primitives.Parser<unit, Filesystem> // the state is always a Dir, never a File.

let spc: Parser = pchar ' ' |>> ignore

let command: Parser =
    let prompt = pchar '$' >>. spc

    let cd =
        prompt .>> pstring "cd" >>. spc >>. restOfLine true
        |>> function
            | "/" -> root
            | ".." -> parent >> Option.get
            | name -> getChild name >> Option.get
        >>= updateUserState

    let ls = prompt .>> pstring "ls" >>. skipRestOfLine true

    attempt cd <|> ls

let entry: Parser =
    let file =
        puint64 .>> spc .>>. restOfLine true |>> File |>> addChild >>= updateUserState

    let dir = pstring "dir" >>. spc >>. restOfLine true |>> addDir >>= updateUserState

    attempt file <|> dir

let rec fold folder state fs =
    let state' = folder state fs

    match fs with
    | File _ -> state'
    | Dir(_, _, cs) -> Seq.fold (fold folder) state' cs

let rec size =
    function
    | File(s, _) -> s
    | Dir(_, _, cs) -> Seq.map size cs |> Seq.reduce (+)

let input =
    runParserOnFile'' (many1 (attempt command <|> entry)) (Dir("/", None, List())) "data/input07.txt"
    |> root

let dirSizes =
    let folder ss =
        function
        | File _ -> ss
        | Dir _ as d -> size d :: ss

    fold folder List.empty

partOne
    "What is the sum of the total sizes of those directories?"
    (input |> dirSizes |> List.filter (fun s -> s <= 100_000UL) |> List.sum)
