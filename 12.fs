open System
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines = File.ReadAllLines(@"input") |> Array.map Array.ofSeq

let ans1 =
    (lines, lines.Length, lines[0].Length)
    |||> fun l n m ->
        [| 0 .. (n - 1) |]
        |> Array.map (fun _ -> Array.create m -1)
        |> fun nl ->
            (Array.allPairs [| 0 .. (n - 1) |] [| 0 .. (m - 1) |])
            |> Array.iter (fun (i, j) ->
                match nl[i][j] with
                | -1 ->
                    [ (i, j) ]
                    |> Array.unfold (fun q ->
                        match q.Length with
                        | 0 -> None
                        | _ ->
                            match q[0] with
                            | (a, b) ->
                                match nl[a][b] <- i * n + j with
                                | _ ->
                                    Some(
                                        0,
                                        ([ (a + 1, b); (a, b + 1); (a - 1, b); (a, b - 1) ]
                                         |> List.filter (fun (X, Y) ->
                                             match X with
                                             | X when X < 0 || X >= n -> false
                                             | _ ->
                                                 match Y with
                                                 | Y when Y < 0 || Y >= m -> false
                                                 | _ ->
                                                     match nl[X][Y] with
                                                     | -1 ->
                                                         match l[X][Y] with
                                                         | c when c = l[i][j] -> true
                                                         | _ -> false
                                                     | _ -> false))
                                        @ q.Tail
                                    ))
                    |> (fun _ -> ())
                | _ -> ())
            |> fun _ ->
                nl
                |> Array.mapi (fun i r -> r |> Array.mapi (fun j x -> (x, (i, j))))
                |> Array.collect id
                |> Array.groupBy fst
                |> Array.Parallel.sumBy (fun (_, fg) ->
                    fg.Length
                    * (4 * fg.Length
                       - (Array.allPairs fg fg
                          |> Array.filter (fun ((_, (i, j)), (_, (x, y))) -> abs (i - x) + abs (j - y) = 1)
                          |> Array.length)))

let ans2 =
    (lines, lines.Length, lines[0].Length)
    |||> fun l n m ->
        [| 0 .. (n - 1) |]
        |> Array.map (fun _ -> Array.create m -1)
        |> fun nl ->
            (Array.allPairs [| 0 .. (n - 1) |] [| 0 .. (m - 1) |])
            |> Array.iter (fun (i, j) ->
                match nl[i][j] with
                | -1 ->
                    [ (i, j) ]
                    |> Array.unfold (fun q ->
                        match q.Length with
                        | 0 -> None
                        | _ ->
                            match q[0] with
                            | (a, b) ->
                                match nl[a][b] <- i * n + j with
                                | _ ->
                                    Some(
                                        0,
                                        ([ (a + 1, b); (a, b + 1); (a - 1, b); (a, b - 1) ]
                                         |> List.filter (fun (X, Y) ->
                                             match X with
                                             | X when X < 0 || X >= n -> false
                                             | _ ->
                                                 match Y with
                                                 | Y when Y < 0 || Y >= m -> false
                                                 | _ ->
                                                     match nl[X][Y] with
                                                     | -1 ->
                                                         match l[X][Y] with
                                                         | c when c = l[i][j] -> true
                                                         | _ -> false
                                                     | _ -> false))
                                        @ q.Tail
                                    ))
                    |> (fun _ -> ())
                | _ -> ())
            |> fun _ ->
                nl
                |> Array.mapi (fun i r -> r |> Array.mapi (fun j x -> (x, (i, j))))
                |> Array.collect id
                |> Array.groupBy fst
                |> Array.Parallel.sumBy (fun (c, fg) ->
                    fg.Length
                    * (([| 0 .. (m - 1) |]
                        |> Array.sumBy (fun j ->
                            [| 0 .. (n - 1) |]
                            |> Array.filter (fun i ->
                                match j with
                                | 0 -> c = nl[i][j]
                                | _ -> c = nl[i][j] && c <> nl[i][j - 1])
                            |> fun ro ->
                                ro |> Array.mapi (fun k v -> (k = 0 || ro[k - 1] + 1 <> v) |> Convert.ToInt32)
                            |> Array.sum))
                       + ([| 0 .. (m - 1) |]
                          |> Array.sumBy (fun j ->
                              [| 0 .. (n - 1) |]
                              |> Array.filter (fun i ->
                                  match j with
                                  | j when j = m - 1 -> c = nl[i][j]
                                  | _ -> c = nl[i][j] && c <> nl[i][j + 1])
                              |> fun ro ->
                                  ro |> Array.mapi (fun k v -> (k = 0 || ro[k - 1] + 1 <> v) |> Convert.ToInt32)
                              |> Array.sum))
                       + ([| 0 .. (n - 1) |]
                          |> Array.sumBy (fun i ->
                              [| 0 .. (m - 1) |]
                              |> Array.filter (fun j ->
                                  match i with
                                  | 0 -> c = nl[i][j]
                                  | _ -> c = nl[i][j] && c <> nl[i - 1][j])
                              |> fun ro ->
                                  ro |> Array.mapi (fun k v -> (k = 0 || ro[k - 1] + 1 <> v) |> Convert.ToInt32)
                              |> Array.sum))
                       + ([| 0 .. (n - 1) |]
                          |> Array.sumBy (fun i ->
                              [| 0 .. (m - 1) |]
                              |> Array.filter (fun j ->
                                  match i with
                                  | i when i = n - 1 -> c = nl[i][j]
                                  | _ -> c = nl[i][j] && c <> nl[i + 1][j])
                              |> fun ro ->
                                  ro |> Array.mapi (fun k v -> (k = 0 || ro[k - 1] + 1 <> v) |> Convert.ToInt32)
                              |> Array.sum))))

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
