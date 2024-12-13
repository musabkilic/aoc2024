open System
open System.IO
open System.Text.RegularExpressions
open System.Numerics

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllText(@"input").Trim().Split("\n\n")
    |> Array.map (fun x ->
        x.Trim().Split("\n")
        |> Array.map (Regex("\\d+").Matches)
        |> Array.map (fun x -> (x[0] |> string |> int, x[1] |> string |> int))
        |> (fun x -> (x[0], x[1], x[2])))

let ans1 =
    lines
    |> Array.sumBy (fun ((xa, ya), (xb, yb), (px, py)) ->
        match xa * yb - xb * ya with
        | d when d = 0 ->
            match
                min
                    (match (px % xa) + (py % ya) with
                     | 0 ->
                         match (px / xa) - (py / ya) with
                         | 0 -> 3 * (px / xa)
                         | _ -> Int32.MaxValue
                     | _ -> Int32.MaxValue)
                    (match (px % xb) + (py % yb) with
                     | 0 ->
                         match (px / xb) - (px / yb) with
                         | 0 -> px / xb
                         | _ -> Int32.MaxValue
                     | _ -> Int32.MaxValue)

            with
            | Int32.MaxValue -> 0
            | x -> x
        | d ->
            match px * yb - py * xb with
            | da when da % d <> 0 -> 0
            | da ->
                match -px * ya + py * xa with
                | db when db % d <> 0 -> 0
                | db -> 3 * (da / d) + (db / d))

let ans2 =
    lines
    |> Array.map (fun ((xa, ya), (xb, yb), (px, py)) ->
        ((xa |> bigint, ya |> bigint),
         (xb |> bigint, yb |> bigint),
         ((px |> bigint) + 10000000000000I, (py |> bigint) + 10000000000000I)))
    |> Array.sumBy (fun ((xa, ya), (xb, yb), (px, py)) ->
        match xa * yb - xb * ya with
        | d when d = 0I ->
            match
                [ (match (px % xa) + (py % ya) with
                   | v when v = 0I ->
                       match (px / xa) - (py / ya) with
                       | v when v = 0I -> 3I * (px / xa)
                       | _ -> 0I
                   | _ -> 0I)
                  (match (px % xb) + (py % yb) with
                   | v when v = 0I ->
                       match (px / xb) - (px / yb) with
                       | v when v = 0I -> px / xb
                       | _ -> 0I
                   | _ -> 0I) ]
                |> List.filter ((<>) 0I)

            with
            | [] -> 0I
            | x -> (x |> List.min)
        | d ->
            match px * yb - py * xb with
            | da when da % d <> 0I -> 0I
            | da ->
                match -px * ya + py * xa with
                | db when db % d <> 0I -> 0I
                | db -> 3I * (da / d) + (db / d))

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
