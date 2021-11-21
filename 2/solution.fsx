open System.IO
open System.Text.RegularExpressions

let INPUT_PATH = "input"
let RANGE_REGEX = @"^([0-9]+)-([0-9]+)$"

let parseRange (inp: string) =
    let result = Regex.Match(inp, RANGE_REGEX)

    match result.Success with
    | true -> (int result.Groups.[1].Value, int result.Groups.[2].Value)
    | false -> failwith "???"


let parse (inp: string []) =
    let range = inp.[0] |> parseRange
    let char = inp.[1].[0]
    let pw = inp.[2]
    (range, char, pw)

let check ((min, max), char, pw) =
    let count =
        pw
        |> String.filter (fun ch -> ch = char)
        |> String.length

    count >= min && count <= max

let check2 ((fst, snd), char, pw: string) =
    match (pw.[fst - 1] = char, pw.[snd - 1] = char) with
    | (true, false)
    | (false, true) -> true
    | _ -> false

let _ =
    INPUT_PATH
    |> File.ReadLines
    |> Seq.map (fun s -> s.Split ' ')
    |> Seq.map parse
    // Uncomment :41 (and comment :42) for the first part.
    // |> Seq.filter check
    |> Seq.filter check2
    |> Seq.length
    |> printfn "%d"
