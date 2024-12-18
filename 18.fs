open System
open System.IO
open System.Text.RegularExpressions

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllText(@"input").Trim().Split("\n")
    |> Array.map (Regex("\\d+").Matches)
    |> Array.map Array.ofSeq
    |> Array.map (Array.map (fun x -> x |> string |> int))
    |> Array.map (fun x -> (x[0], x[1]))

let ans1 =
    lines
    |> fun l ->
        [| 0..70 |]
        |> Array.map (fun _ -> Array.create 71 Int32.MaxValue)
        |> fun g ->
            [| 0..1023 |]
            |> Array.iter (fun i ->
                match g[snd l[i]][fst l[i]] <- -1 with
                | _ -> ())
            |> fun _ ->
                [ (Set.empty, (0, 0)) ]
                |> Array.unfold (fun q ->
                    match q.Length with
                    | 0 -> None
                    | _ ->
                        match q[0] with
                        | (v, (x, y)) ->
                            match (x, y) with
                            | (70, 70) -> Some(v.Count, q.Tail)
                            | _ ->
                                match v.Count with
                                | sc when sc >= g[x][y] -> Some(Int32.MaxValue, q.Tail)
                                | sc ->
                                    match (g[x][y] <- sc) with
                                    | _ ->
                                        Some(
                                            Int32.MaxValue,
                                            q.Tail
                                            @ ([ (-1, 0); (1, 0); (0, -1); (0, 1) ]
                                               |> List.filter (fun (tx, ty) ->
                                                   match x + tx with
                                                   | x when x < 0 || x >= 71 -> false
                                                   | _ ->
                                                       match y + ty with
                                                       | y when y < 0 || y >= 71 -> false
                                                       | _ ->
                                                           match g[x + tx][y + ty] with
                                                           | -1 -> false
                                                           | _ ->
                                                               sc < g[x + tx][y + ty]
                                                               && (not (v.Contains((x + tx, y + ty)))))
                                               |> List.map (fun (tx, ty) -> (v.Add((x, y)), (x + tx, y + ty))))
                                        ))
                |> Array.min


let ans2 =
    lines
    |> fun l ->
        (0, l.Length - 1)
        |> Array.unfold (fun (a, b) ->
            match a with
            | a when a >= b -> None
            | _ ->
                [| 0..70 |]
                |> Array.map (fun _ -> Array.create 71 Int32.MaxValue)
                |> fun g ->
                    [| 0 .. ((a + b) / 2) |]
                    |> Array.iter (fun i ->
                        match g[snd l[i]][fst l[i]] <- -1 with
                        | _ -> ())
                    |> fun _ ->
                        match
                            (Set.empty, Set.empty.Add((0, 0)), true)
                            |> Array.unfold (fun (v, q, con) ->
                                match con with
                                | false -> None
                                | true ->
                                    match q.Count with
                                    | 0 -> Some(v.Contains((70, 70)), (Set.empty, Set.empty, false))
                                    | _ ->
                                        match q |> Set.minElement with
                                        | (x, y) ->
                                            Some(
                                                false,
                                                (v.Add((x, y)),
                                                 q.Remove((x, y))
                                                 |> Set.union (
                                                     ([ (-1, 0); (1, 0); (0, -1); (0, 1) ]
                                                      |> List.filter (fun (tx, ty) ->
                                                          match x + tx with
                                                          | x when x < 0 || x >= 71 -> false
                                                          | _ ->
                                                              match y + ty with
                                                              | y when y < 0 || y >= 71 -> false
                                                              | _ ->
                                                                  match g[x + tx][y + ty] with
                                                                  | -1 -> false
                                                                  | _ -> not (v.Contains((x + tx, y + ty))))
                                                      |> List.map (fun (tx, ty) -> (x + tx, y + ty))
                                                      |> Set.ofSeq)
                                                 ),
                                                 con)
                                            ))
                            |> Array.last
                        with
                        | true -> Some(((a + b) / 2 + 1, b), ((a + b) / 2 + 1, b))
                        | false -> Some((a, (a + b) / 2), (a, (a + b) / 2)))
        |> Array.last
        |> fst
        |> fun i -> [| fst l[i] |> string; snd l[i] |> string |] |> String.concat ","

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
