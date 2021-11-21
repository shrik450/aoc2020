open System.IO

let INPUT_PATH = "input"

/// Super simple solution for the first half.
/// You *can* make it work for the second one, but it loses the elegance of
/// the solution.
let firstHalf =
    INPUT_PATH
    |> File.ReadLines
    |> Seq.mapi (fun idx line ->
        let currentIndex = idx * 3 % String.length line
        line.[currentIndex])
    |> Seq.filter (fun ch -> ch = '#')
    |> Seq.length
// |> printfn "%d"

let count right down ch arr =
    let max = Array.length arr
    let lineLength = String.length arr.[0]

    let rec _count idx idy acc =
        if idy >= max then
            acc
        else
            let newAcc =
                if arr.[idy].[idx] = ch then
                    acc + 1
                else
                    acc

            _count ((idx + right) % lineLength) (idy + down) newAcc

    _count 0 0 0

let checks =
    [ (1, 1)
      (3, 1)
      (5, 1)
      (7, 1)
      (1, 2) ]

let countAll arr =
    List.map (fun (right, down) -> count right down '#' arr) checks

let _ =
    INPUT_PATH
    |> File.ReadLines
    |> Seq.toArray
    |> countAll
    |> List.map int64
    |> List.fold (*) 1L
    |> printfn "%d"
