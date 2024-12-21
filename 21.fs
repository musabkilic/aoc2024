open System
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines = File.ReadAllText(@"input").Trim().Split("\n")

let ans1 =
    Array.allPairs [| 0..2 |] [| 0..1 |]
    |> Array.filter ((<>) (0, 0))
    |> fun rc ->
        Array.allPairs rc rc
        |> Array.map (fun ((ax, ay), (bx, by)) -> (((ax, ay), (bx, by)), abs (ax - bx) + abs (ay - by)))
        |> Map
        |> fun l0 ->
            Array.allPairs rc rc
            |> Array.map (fun ((ax, ay), (bx, by)) ->
                [ (ax, ay, (2, 0), 0) ]
                |> Array.unfold (fun q ->
                    match q.Length with
                    | 0 -> None
                    | _ ->
                        match q[0] with
                        | (x, y, pd, s) when (x, y) = (bx, by) -> Some(s + l0[(pd, (2, 0))], q.Tail)
                        | (x, y, pd, s) ->
                            Some(
                                Int32.MaxValue,
                                q.Tail
                                @ ([ (0, sign (by - y)); (sign (bx - x), 0) ]
                                   |> List.filter (fun (dx, dy) ->
                                       match (dx + x, dy + y) with
                                       | (0, 0) -> false
                                       | v when v = (x, y) -> false
                                       | _ -> true)
                                   |> List.map (fun (dx, dy) ->
                                       match
                                           match (dx, dy) with
                                           | (0, 1) -> (1, 1)
                                           | (0, -1) -> (1, 0)
                                           | (1, 0) -> (2, 1)
                                           | _ -> (0, 1)
                                       with
                                       | nd -> (x + dx, y + dy, nd, s + 1 + l0[(pd, nd)])))
                            ))
                |> Array.min
                |> fun m -> (((ax, ay), (bx, by)), m))
        |> Map
    |> fun l1 ->
        Array.allPairs [| 0..2 |] [| 0..3 |]
        |> Array.filter ((<>) (0, 3))
        |> fun rc ->
            Array.allPairs rc rc
            |> Array.map (fun ((ax, ay), (bx, by)) ->
                [ (ax, ay, (2, 0), 0) ]
                |> Array.unfold (fun q ->
                    match q.Length with
                    | 0 -> None
                    | _ ->
                        match q[0] with
                        | (x, y, pd, s) when (x, y) = (bx, by) -> Some(s + l1[(pd, (2, 0))], q.Tail)
                        | (x, y, pd, s) ->
                            Some(
                                Int32.MaxValue,
                                q.Tail
                                @ ([ (0, sign (by - y)); (sign (bx - x), 0) ]
                                   |> List.filter (fun (dx, dy) ->
                                       match (dx + x, dy + y) with
                                       | (0, 3) -> false
                                       | v when v = (x, y) -> false
                                       | _ -> true)
                                   |> List.map (fun (dx, dy) ->
                                       match
                                           match (dx, dy) with
                                           | (0, 1) -> (1, 1)
                                           | (0, -1) -> (1, 0)
                                           | (1, 0) -> (2, 1)
                                           | _ -> (0, 1)
                                       with
                                       | nd -> (x + dx, y + dy, nd, s + 1 + l1[(pd, nd)])))
                            ))
                |> Array.min
                |> fun m -> (((ax, ay), (bx, by)), m))
    |> Map
    |> fun M ->
        lines
        |> Array.sumBy (fun r ->
            ("A" + r)
            |> Array.ofSeq
            |> Array.map (fun c ->
                match c with
                | '7' -> (0, 0)
                | '8' -> (1, 0)
                | '9' -> (2, 0)
                | '4' -> (0, 1)
                | '5' -> (1, 1)
                | '6' -> (2, 1)
                | '1' -> (0, 2)
                | '2' -> (1, 2)
                | '3' -> (2, 2)
                | '0' -> (1, 3)
                | _ -> (2, 3))
            |> Array.pairwise
            |> Array.sumBy (fun x -> M[x] + 1)
            |> fun i -> i * (r.Substring(0, 3) |> int))

let ans2 =
    Array.allPairs [| 0..2 |] [| 0..1 |]
    |> Array.filter ((<>) (0, 0))
    |> fun rc ->
        Array.allPairs rc rc
        |> Array.map (fun ((ax, ay), (bx, by)) -> (((ax, ay), (bx, by)), (abs (ax - bx) + abs (ay - by)) |> int64))
        |> Map
        |> fun l0 ->
            (l0, [| 0..23 |])
            ||> Array.fold (fun l0 k ->
                Array.allPairs rc rc
                |> Array.map (fun ((ax, ay), (bx, by)) ->
                    [ (ax, ay, (2, 0), 0L) ]
                    |> Array.unfold (fun q ->
                        match q.Length with
                        | 0 -> None
                        | _ ->
                            match q[0] with
                            | (x, y, pd, s) when (x, y) = (bx, by) -> Some(s + l0[(pd, (2, 0))], q.Tail)
                            | (x, y, pd, s) ->
                                Some(
                                    -1L,
                                    q.Tail
                                    @ ([ (0, sign (by - y)); (sign (bx - x), 0) ]
                                       |> List.filter (fun (dx, dy) ->
                                           match (dx + x, dy + y) with
                                           | (0, 0) -> false
                                           | v when v = (x, y) -> false
                                           | _ -> true)
                                       |> List.map (fun (dx, dy) ->
                                           match
                                               match (dx, dy) with
                                               | (0, 1) -> (1, 1)
                                               | (0, -1) -> (1, 0)
                                               | (1, 0) -> (2, 1)
                                               | _ -> (0, 1)
                                           with
                                           | nd -> (x + dx, y + dy, nd, s + 1L + l0[(pd, nd)])))
                                ))
                    |> Array.filter ((<>) -1L)
                    |> Array.min
                    |> fun m -> (((ax, ay), (bx, by)), m))
                |> Map)
    |> fun l1 ->
        Array.allPairs [| 0..2 |] [| 0..3 |]
        |> Array.filter ((<>) (0, 3))
        |> fun rc ->
            Array.allPairs rc rc
            |> Array.map (fun ((ax, ay), (bx, by)) ->
                [ (ax, ay, (2, 0), 0L) ]
                |> Array.unfold (fun q ->
                    match q.Length with
                    | 0 -> None
                    | _ ->
                        match q[0] with
                        | (x, y, pd, s) when (x, y) = (bx, by) -> Some(s + l1[(pd, (2, 0))], q.Tail)
                        | (x, y, pd, s) ->
                            Some(
                                -1L,
                                q.Tail
                                @ ([ (0, sign (by - y)); (sign (bx - x), 0) ]
                                   |> List.filter (fun (dx, dy) ->
                                       match (dx + x, dy + y) with
                                       | (0, 3) -> false
                                       | v when v = (x, y) -> false
                                       | _ -> true)
                                   |> List.map (fun (dx, dy) ->
                                       match
                                           match (dx, dy) with
                                           | (0, 1) -> (1, 1)
                                           | (0, -1) -> (1, 0)
                                           | (1, 0) -> (2, 1)
                                           | _ -> (0, 1)
                                       with
                                       | nd -> (x + dx, y + dy, nd, s + 1L + l1[(pd, nd)])))
                            ))
                |> Array.filter ((<>) -1L)
                |> Array.min
                |> fun m -> (((ax, ay), (bx, by)), m))
    |> Map
    |> fun M ->
        lines
        |> Array.sumBy (fun r ->
            ("A" + r)
            |> Array.ofSeq
            |> Array.map (fun c ->
                match c with
                | '7' -> (0, 0)
                | '8' -> (1, 0)
                | '9' -> (2, 0)
                | '4' -> (0, 1)
                | '5' -> (1, 1)
                | '6' -> (2, 1)
                | '1' -> (0, 2)
                | '2' -> (1, 2)
                | '3' -> (2, 2)
                | '0' -> (1, 3)
                | _ -> (2, 3))
            |> Array.pairwise
            |> Array.sumBy (fun x -> M[x] + 1L)
            |> fun i -> i * (r.Substring(0, 3) |> int64))

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
