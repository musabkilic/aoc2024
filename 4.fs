open System
open System.IO
open System.Text.RegularExpressions

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllLines(@"input")
    |> fun x -> Array2D.init x.Length x[0].Length (fun i j -> x[i][j] |> string)


let ans1 =
    lines
    |> fun l ->
        (Array.init (l |> Array2D.length1) (fun i -> l[i, *] |> String.concat "")
         |> Array.sumBy (fun x -> (x |> Regex("XMAS").Count) + (x |> Regex("SAMX").Count)))
        + (Array.init (l |> Array2D.length1) (fun i -> l[*, i] |> String.concat "")
           |> Array.sumBy (fun x -> (x |> Regex("XMAS").Count) + (x |> Regex("SAMX").Count)))
        + (Array.init (2 * (l |> Array2D.length1) - 1) (fun i ->
            Array.init ((l |> Array2D.length1) - abs (i - (l |> Array2D.length1) + 1)) (fun j ->
                match i with
                | i when i < (l |> Array2D.length1) -> l[j, i - j]
                | _ -> l[i - (l |> Array2D.length1) + 1 + j, (l |> Array2D.length1) - 1 - j])
            |> String.concat "")
           |> Array.sumBy (fun x -> (x |> Regex("XMAS").Count) + (x |> Regex("SAMX").Count)))
        + (Array.init (2 * (l |> Array2D.length1) - 1) (fun i ->
            Array.init ((l |> Array2D.length1) - abs (i - (l |> Array2D.length1) + 1)) (fun j ->
                match i with
                | i when i < (l |> Array2D.length1) -> l[(l |> Array2D.length1) - 1 - i + j, j]
                | _ -> l[j, i - (l |> Array2D.length1) + 1 + j])
            |> String.concat "")
           |> Array.sumBy (fun x -> (x |> Regex("XMAS").Count) + (x |> Regex("SAMX").Count)))

let ans2 =
    lines
    |> fun l ->
        l
        |> Array2D.mapi (fun i j v ->
            match v with
            | "A" ->
                match i with
                | 0 -> 0
                | i when i = (l |> Array2D.length1) - 1 -> 0
                | _ ->
                    match j with
                    | 0 -> 0
                    | i when i = (l |> Array2D.length2) - 1 -> 0
                    | _ ->
                        match (l[i - 1, j - 1], l[i - 1, j + 1], l[i + 1, j - 1], l[i + 1, j + 1]) with
                        | ("M", "M", "S", "S") -> 1
                        | ("M", "S", "M", "S") -> 1
                        | ("S", "S", "M", "M") -> 1
                        | ("S", "M", "S", "M") -> 1
                        | _ -> 0
            | _ -> 0)
        |> Seq.cast<int>
        |> Seq.sum

printfn "%A %A" ans1 ans2
