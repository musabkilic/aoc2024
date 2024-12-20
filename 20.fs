open System
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines = File.ReadAllText(@"input").Trim().Split("\n") |> Array.map Array.ofSeq

let ans1 =
    (lines, lines.Length, lines[0].Length)
    |||> fun l n m ->
        Array.allPairs [| 0 .. (n - 1) |] [| 0 .. (m - 1) |]
        |> Array.find (fun (x, y) ->
            match l[x][y] with
            | 'S' -> true
            | _ -> false)
        |> fun (sx, sy) -> (sx, sy, sx, sy)
        |> Array.unfold (fun (x, y, bx, by) ->
            match (x, y) with
            | (-1, -1) -> None
            | _ ->
                match l[x][y] with
                | 'E' -> Some((x, y), (-1, -1, -1, -1))
                | _ ->
                    Some(
                        (x, y),
                        ([ (0, 1); (0, -1); (1, 0); (-1, 0) ]
                         |> List.find (fun (dx, dy) ->
                             match x + dx with
                             | x when x < 0 || x >= n -> false
                             | _ ->
                                 match y + dy with
                                 | y when y < 0 || y >= m -> false
                                 | _ ->
                                     match (x + dx, y + dy) with
                                     | p when p = (bx, by) -> false
                                     | _ ->
                                         match l[x + dx][y + dy] with
                                         | '#' -> false
                                         | _ -> true)
                         |> fun (dx, dy) -> (x + dx, y + dy, x, y))
                    ))
        |> fun r ->
            r
            |> Array.Parallel.mapi (fun i (x0, y0) ->
                r
                |> Array.mapi (fun j (x1, y1) ->
                    match j with
                    | j when j > i ->
                        match (pown (x0 - x1) 2) + (pown (y0 - y1) 2) with
                        | 4 -> j - i - 2
                        | _ -> 0
                    | _ -> 0)
                |> Array.filter (fun x -> x >= 100)
                |> Array.length)
            |> Array.sum



let ans2 =
    (lines, lines.Length, lines[0].Length)
    |||> fun l n m ->
        Array.allPairs [| 0 .. (n - 1) |] [| 0 .. (m - 1) |]
        |> Array.find (fun (x, y) ->
            match l[x][y] with
            | 'S' -> true
            | _ -> false)
        |> fun (sx, sy) -> (sx, sy, sx, sy)
        |> Array.unfold (fun (x, y, bx, by) ->
            match (x, y) with
            | (-1, -1) -> None
            | _ ->
                match l[x][y] with
                | 'E' -> Some((x, y), (-1, -1, -1, -1))
                | _ ->
                    Some(
                        (x, y),
                        ([ (0, 1); (0, -1); (1, 0); (-1, 0) ]
                         |> List.find (fun (dx, dy) ->
                             match x + dx with
                             | x when x < 0 || x >= n -> false
                             | _ ->
                                 match y + dy with
                                 | y when y < 0 || y >= m -> false
                                 | _ ->
                                     match (x + dx, y + dy) with
                                     | p when p = (bx, by) -> false
                                     | _ ->
                                         match l[x + dx][y + dy] with
                                         | '#' -> false
                                         | _ -> true)
                         |> fun (dx, dy) -> (x + dx, y + dy, x, y))
                    ))
        |> fun r ->
            r
            |> Array.Parallel.mapi (fun i (x0, y0) ->
                r
                |> Array.mapi (fun j (x1, y1) ->
                    match j with
                    | j when j > i ->
                        match abs (x0 - x1) + abs (y0 - y1) with
                        | x when x <= 20 -> j - i - x
                        | _ -> 0
                    | _ -> 0)
                |> Array.filter (fun x -> x >= 100)
                |> Array.length)
            |> Array.sum


stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
