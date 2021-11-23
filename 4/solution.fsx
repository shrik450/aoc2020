open System.IO
open System.Text.RegularExpressions

let INPUT_PATH = "input"

let pattern = @"\b([a-z]{3}):(\S+)\b"

/// Super simple solution for the first half.
/// Of course, this too doesn't work for the second half.
let simpleValid line =
    let matches = Regex.Matches(line, pattern)

    match matches.Count with
    | 8 -> true
    | 7 ->
        matches
        |> Seq.map (fun (x: Match) -> x.Groups.[1].Value)
        |> Seq.contains "cid"
        |> not
    | _ -> false

let simple _ =
    use file = new StreamReader(INPUT_PATH)
    let contents = file.ReadToEnd()

    contents.Split("\n\n")
    |> Array.fold
        (fun count line ->
            if simpleValid line then
                count + 1
            else
                count)
        0
    |> printfn "%d"

let validateByr value =
    (String.length value = 4)
    && (value <= "2002")
    && (value >= "1920")

let validateIyr value =
    (String.length value = 4)
    && (value <= "2020")
    && (value >= "2010")

let validateEyr value =
    (String.length value = 4)
    && (value <= "2030")
    && (value >= "2020")

let validateHgt value =
    let mat =
        Regex.Match(value, @"^([0-9]{2,3})(cm|in)$")

    mat.Success
    && (let (height, measure) =
            (int mat.Groups.[1].Value, mat.Groups.[2].Value)

        match measure with
        | "in" -> height >= 59 && height <= 76
        | "cm" -> height >= 150 && height <= 193
        | _ -> false)

let validateHcl value =
    Regex.Match(value, @"^#[a-f0-9]{6}$").Success

let ecls =
    set [ "amb"
          "blu"
          "brn"
          "gry"
          "grn"
          "hzl"
          "oth" ]

let validateEcl value = Set.contains value ecls

let validatePid value =
    Regex.Match(value, @"^[0-9]{9}$").Success

let validations =
    dict [ "byr", validateByr
           "iyr", validateIyr
           "eyr", validateEyr
           "hgt", validateHgt
           "hcl", validateHcl
           "ecl", validateEcl
           "pid", validatePid
           "cid", (fun _ -> true) ]

let valid line =
    let matches = Regex.Matches(line, pattern)

    matches
    |> Seq.map (fun mat -> (mat.Groups.[1].Value, mat.Groups.[2].Value))
    |> Map.ofSeq
    // Since we're ignoring cid anyway, we can change its value if it's
    // already present.
    |> Map.add "cid" ""
    |> Map.map (fun field value -> validations.Item field value)
    |> Seq.sumBy (fun pair -> if pair.Value then 1 else 0)
    |> (=) 8

let _ =
    use file = new StreamReader(INPUT_PATH)
    let contents = file.ReadToEnd()

    contents.Split("\n\n")
    |> Array.filter valid
    |> Array.length
    |> printfn "%d"
