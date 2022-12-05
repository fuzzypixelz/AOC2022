#r "nuget: FParsec"
open FParsec

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
        | Success(i, _, _) -> i
        | Failure(e, _, _) -> failwith e
