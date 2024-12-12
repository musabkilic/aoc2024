open System
open System.IO
open System.Numerics

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines = File.ReadAllLines(@"input")

let ans1 =
    (lines.Length, lines[0].Length)
    |> (fun (N, M) ->
        lines
        |> Array.mapi (fun x r -> r.ToCharArray() |> Array.mapi (fun y v -> (v, (x, y))))
        |> Array.collect id
        |> Array.groupBy fst
        |> Array.filter (fun (c, a) -> c <> '.')
        |> Array.map (fun (c, a) ->
            Array.allPairs a a
            |> Array.filter (fun (ai, aj) -> ai <> aj)
            |> Array.map (fun ((_, (x1, y1)), (_, (x2, y2))) ->
                [| (x1 + 2 * (x2 - x1), y1 + 2 * (y2 - y1))
                   (x2 + 2 * (x1 - x2), y2 + 2 * (y1 - y2)) |]
                |> Array.filter (fun (a, b) -> a >= 0 && a < N && b >= 0 && b < M))
            |> Array.collect id))
    |> Array.collect id
    |> Array.groupBy id
    |> Array.length

let ans2 =
    (lines.Length, lines[0].Length)
    |> (fun (N, M) ->
        lines
        |> Array.mapi (fun x r -> r.ToCharArray() |> Array.mapi (fun y v -> (v, (x, y))))
        |> Array.collect id
        |> Array.groupBy fst
        |> Array.filter (fun (c, a) -> c <> '.')
        |> Array.map (fun (c, a) ->
            Array.allPairs a a
            |> Array.filter (fun (ai, aj) -> ai <> aj)
            |> Array.map (fun ((_, (x1, y1)), (_, (x2, y2))) ->
                (1
                 |> Array.unfold (fun i ->
                     match (x1 + i * (x2 - x1), y1 + i * (y2 - y1)) with
                     | (a, b) when a >= 0 && a < N && b >= 0 && b < M -> Some((a, b), i + 1)
                     | _ -> None),
                 1
                 |> Array.unfold (fun i ->
                     match (x2 + i * (x1 - x2), y2 + i * (y1 - y2)) with
                     | (a, b) when a >= 0 && a < N && b >= 0 && b < M -> Some((a, b), i + 1)
                     | _ -> None))
                ||> Array.append)
            |> Array.collect id))
    |> Array.collect id
    |> Array.groupBy id
    |> Array.length

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
