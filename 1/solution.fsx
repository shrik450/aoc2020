open System.IO

let INPUT_PATH = "input.txt"

let pickPair sum (inp: int []) =

    let rec traverse left right =
        if left = right then
            None
        else
            match inp.[left] + inp.[right] with
            | x when x < sum -> traverse (left + 1) right
            | x when x = sum -> Some(inp.[left], inp.[right])
            | x when x > sum -> traverse left (right - 1)
            | _ -> failwith "this just gets rid of the warning"

    traverse 0 (Array.length inp - 1)

let pickTrip sum (inp: int []) =
    // This could be optimized further by eliminating elements on the right
    // such that left + right > sum
    let rec traverse left right =
        let newInp = inp.[left + 1..right]
        let newSum = sum - inp.[left]

        match pickPair newSum newInp with
        | Some pair -> (inp.[left], fst pair, snd pair)
        | None -> traverse (left + 1) right

    traverse 0 (Array.length inp - 1)

let mult (a, b) = a * b
let multT (a, b, c) = a * b * c

let _ =
    INPUT_PATH
    |> File.ReadLines
    |> Seq.map int
    |> Seq.toArray
    |> Array.sort
    // Uncomment these lines (and comment :45..46) for the first part
    // |> pickPair 2020
    // |> Option.get
    // |> mult
    |> pickTrip 2020
    |> multT
    |> printfn "%d"
