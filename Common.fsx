#r "nuget: FParsec"
open FParsec

let flip f x y = f y x

let answer part question value =
    printfn "(Part %d) %s" part question
    printfn "(Answer) %A" value
    value

let partOne q v = answer 1 q v
let partTwo q v = answer 2 q v

type Parser<'a> = Parser<'a, unit>

let runParserOnFile' parser file =
    runParserOnFile parser () file (System.Text.UTF8Encoding())
    |> function
        | Success(r, _, _) -> r
        | Failure(_, e, _) -> failwithf "%A" e

let runParserOnFile'' parser state file =
    runParserOnFile parser state file (System.Text.UTF8Encoding())
    |> function
        | Success(_, s, _) -> s
        | Failure(_, e, _) -> failwithf "%A" e
