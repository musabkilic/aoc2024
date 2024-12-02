open System
open System.IO

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllLines(@"input")
    |> Array.map (fun x -> x.Split(" ") |> Array.map (fun x -> x |> int))

let ans1 =
    lines
    |> Array.filter (fun l ->
        (((System.Int32.MaxValue, System.Int32.MinValue), l |> Array.pairwise |> Array.map (fun (x, y) -> y - x)))
        ||> Array.fold (fun (mi, ma) x -> (mi |> min x, ma |> max x))
        |> fun (mi, ma) -> (mi >= 1 && ma <= 3) || (mi >= -3 && ma <= -1))
    |> Array.length

let ans2 =
    lines
    |> Array.filter (fun l ->
        [| 0 .. (Array.length l - 1) |]
        |> Array.map (fun x -> (l |> Array.removeAt (x)))
        |> Array.exists (fun nl ->
            (((System.Int32.MaxValue, System.Int32.MinValue), nl |> Array.pairwise |> Array.map (fun (x, y) -> y - x)))
            ||> Array.fold (fun (mi, ma) x -> (mi |> min x, ma |> max x))
            |> fun (mi, ma) -> (mi >= 1 && ma <= 3) || (mi >= -3 && ma <= -1)))
    |> Array.length

printfn "%i %i" ans1 ans2
