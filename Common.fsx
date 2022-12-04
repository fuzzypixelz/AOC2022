#r "nuget: FParsec"
open FParsec

let answer part question value =
    printfn "(Part %d) %s" part question
    printfn "(Answer) %A" value
    value

let partOne q v = answer 1 q v
let partTwo q v = answer 2 q v

let runParserOnFile' parser file =
    runParserOnFile parser () file (System.Text.UTF8Encoding())
    |> function
        | Success(i, _, _) -> Array.ofList i
        | Failure(e, _, _) -> failwith e
