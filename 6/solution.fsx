open System.IO
open System

let INPUT_PATH = "input"

let total (group: string) =
    group.ToCharArray()
    |> Seq.filter (fun char -> Char.IsLetter char)
    |> Set.ofSeq
    |> Set.count

let fullSet = set [ 'a' .. 'z' ]

let answeredQuestions (person: String) = person.ToCharArray() |> Set.ofArray

let allYes (group: string) =
    group.Split("\n")
    |> Seq.filter (fun line -> String.length line <> 0)
    |> Seq.fold (fun all person -> person |> answeredQuestions |> Set.intersect all) fullSet
    |> Set.count

let _ =
    use file = new StreamReader(INPUT_PATH)
    let contents = file.ReadToEnd()

    contents.Split("\n\n")
    // Uncomment for first half
    // |> Seq.map total
    |> Seq.map allYes
    |> Seq.sum
    |> printfn "%d"
