open System
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines = File.ReadAllText(@"input").Trim().Split(" ") |> List.ofSeq

let ans1 =
    (lines |> List.countBy id, [ 1..25 ])
    ||> List.fold (fun l _ ->
        (l, [])
        ||> List.foldBack (fun (r, c) nl ->
            match (r, r.Length) with
            | ("0", 1) -> ("1", c) :: nl
            | (r, n) when n % 2 = 0 ->
                (r.Substring(0, n / 2), c)
                :: (match r.Substring(n / 2).TrimStart('0') with
                    | "" -> ("0", c)
                    | x -> (x, c))
                :: nl
            | _ -> (string ((int64 r) * 2024L), c) :: nl)
        |> List.groupBy fst
        |> List.map (fun (k, v) -> (k, v |> List.sumBy snd)))
    |> List.sumBy snd

let ans2 =
    (lines |> List.countBy id |> List.map (fun (k, v) -> (k, v |> int64)), [ 1..75 ])
    ||> List.fold (fun l _ ->
        (l, [])
        ||> List.foldBack (fun (r, c) nl ->
            match (r, r.Length) with
            | ("0", 1) -> ("1", c) :: nl
            | (r, n) when n % 2 = 0 ->
                (r.Substring(0, n / 2), c)
                :: (match r.Substring(n / 2).TrimStart('0') with
                    | "" -> ("0", c)
                    | x -> (x, c))
                :: nl
            | _ -> (string ((int64 r) * 2024L), c) :: nl)
        |> List.groupBy fst
        |> List.map (fun (k, v) -> (k, v |> List.sumBy snd)))
    |> List.sumBy snd

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
