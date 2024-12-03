open System
open System.IO
open System.Text.RegularExpressions

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let text = File.ReadAllText(@"input")

let ans1 =
    Regex.Matches(text, "mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)")
    |> Array.ofSeq
    |> Array.map (fun x ->
        x.Value[4 .. x.Value.Length - 2].Split(",")
        |> Array.map (int)
        |> fun y -> y[0] * y[1])
    |> Array.sum

let ans2 =
    (true,
     Regex.Matches(text, "(mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)|don't\\(\\)|do\\(\\))")
     |> Array.ofSeq)
    ||> Array.mapFold (fun d x ->
        match x.Value with
        | "do()" -> 0, true
        | "don't()" -> 0, false
        | _ when d ->
            (x.Value[4 .. x.Value.Length - 2].Split(",")
             |> Array.map (int)
             |> fun y -> y[0] * y[1]),
            d
        | _ -> 0, d)
    |> fun (a, b) -> Array.sum (a)


printfn "%A %A" ans1 ans2
