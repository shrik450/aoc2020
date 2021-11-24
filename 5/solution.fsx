open System.IO

let INPUT_PATH = "input"

type Half =
    | Upper
    | Lower

let step (low, high) half =
    if low >= high then
        (low, low)
    else
        let mid = (low + high) / 2
        // printfn "%d" mid

        match half with
        | Upper -> (mid + 1, high)
        | Lower -> (low, mid)

let find (line: string) =
    let arr = line.ToCharArray()
    let rows = arr.[..6]
    let cols = arr.[7..]

    let row =
        rows
        |> Array.fold
            (fun pair char ->
                let half =
                    match char with
                    | 'F' -> Lower
                    | 'B' -> Upper
                    | _ -> failwith "???"

                step pair half)
            (0, 127)
        |> fst

    let col =
        cols
        |> Array.fold
            (fun pair char ->
                let half =
                    match char with
                    | 'R' -> Upper
                    | 'L' -> Lower
                    | _ -> failwith "???"

                step pair half)
            (0, 7)
        |> fst

    (row, col)

let _ =
    INPUT_PATH
    |> File.ReadLines
    |> Seq.map find
    |> Seq.map (fun (x, y) -> (x * 8) + y)
    // Solution for the first half
    // |> Seq.max
    // |> printfn "%d"
    |> Seq.sort
    // Really exploiting knowledge of the problem here.
    |> Seq.iteri (fun idx id -> if idx <> (id - 12) then printfn "%d" id)
