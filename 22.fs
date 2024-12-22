open System
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines = File.ReadAllText(@"input").Trim().Split("\n") |> Array.map int64

let ans1 =
    lines
    |> Array.sumBy (fun i ->
        (i, [| 1..2000 |])
        ||> Array.fold (fun x _ ->
            match ((x <<< 6) ^^^ x) % 16777216L with
            | s1 ->
                match ((s1 >>> 5) ^^^ s1) % 16777216L with
                | s2 -> ((s2 <<< 11) ^^^ s2) % 16777216L))

let ans2 =
    lines
    |> Array.Parallel.map (fun i ->
        (i, [| 1..2000 |])
        ||> Array.scan (fun x _ ->
            match ((x <<< 6) ^^^ x) % 16777216L with
            | s1 ->
                match ((s1 >>> 5) ^^^ s1) % 16777216L with
                | s2 -> ((s2 <<< 11) ^^^ s2) % 16777216L)
        |> Array.map (fun x -> (x % 10L) |> int)
        |> Array.pairwise
        |> Array.map (fun (a, b) -> (b, b - a))
        |> Array.windowed 4
        |> Array.map (fun x -> ((snd x[0], snd x[1], snd x[2], snd x[3]), fst x[3]))
        |> Array.rev
        |> Map)
    |> fun M ->
        M
        |> Array.map (fun x -> x |> Map.keys |> Set.ofSeq)
        |> Set.unionMany
        |> Array.ofSeq
        |> Array.maxBy (fun k ->
            M
            |> Array.sumBy (fun m ->
                match m |> Map.tryFind k with
                | Some x -> x
                | None -> 0))
        |> Array.max)

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
