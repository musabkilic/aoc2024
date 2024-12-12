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
        l
        |> Array2D.mapi (fun i j v ->
            match j with
            | j when j > (l |> Array2D.length2) - 4 -> 0
            | _ ->
                match v with
                | "X" ->
                    match (l[i, j + 1], l[i, j + 2], l[i, j + 3]) with
                    | ("M", "A", "S") -> 1
                    | _ -> 0
                | "S" ->
                    match (l[i, j + 1], l[i, j + 2], l[i, j + 3]) with
                    | ("A", "M", "X") -> 1
                    | _ -> 0
                | _ -> 0
            + match i with
              | i when i > (l |> Array2D.length2) - 4 -> 0
              | _ ->
                  match v with
                  | "X" ->
                      match (l[i + 1, j], l[i + 2, j], l[i + 3, j]) with
                      | ("M", "A", "S") -> 1
                      | _ -> 0
                  | "S" ->
                      match (l[i + 1, j], l[i + 2, j], l[i + 3, j]) with
                      | ("A", "M", "X") -> 1
                      | _ -> 0
                  | _ -> 0
            + match i with
              | i when i > (l |> Array2D.length2) - 4 -> 0
              | _ ->
                  match j with
                  | j when j > (l |> Array2D.length2) - 4 -> 0
                  | _ ->
                      match v with
                      | "X" ->
                          match (l[i + 1, j + 1], l[i + 2, j + 2], l[i + 3, j + 3]) with
                          | ("M", "A", "S") -> 1
                          | _ -> 0
                      | "S" ->
                          match (l[i + 1, j + 1], l[i + 2, j + 2], l[i + 3, j + 3]) with
                          | ("A", "M", "X") -> 1
                          | _ -> 0
                      | _ -> 0
            + match i with
              | i when i < 3 -> 0
              | _ ->
                  match j with
                  | j when j > (l |> Array2D.length2) - 4 -> 0
                  | _ ->
                      match v with
                      | "X" ->
                          match (l[i - 1, j + 1], l[i - 2, j + 2], l[i - 3, j + 3]) with
                          | ("M", "A", "S") -> 1
                          | _ -> 0
                      | "S" ->
                          match (l[i - 1, j + 1], l[i - 2, j + 2], l[i - 3, j + 3]) with
                          | ("A", "M", "X") -> 1
                          | _ -> 0
                      | _ -> 0)
        |> Seq.cast<int>
        |> Seq.sum

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
