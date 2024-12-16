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
        l
        |> Array.map Array.copy
        |> Array.mapi (fun i r ->
            r
            |> Array.mapi (fun j x ->
                match x with
                | 'S' -> i * n + j
                | _ -> 0)
            |> Array.sum)
        |> Array.sum
        |> fun x -> (x / n, x % n)
        ||> fun sx sy ->
            [| 0 .. (n - 1) |]
            |> Array.map (fun _ -> Array.create m Int32.MaxValue)
            |> fun nl ->
                [ (Set.empty, (sx, sy), (0, 1), 0) ]
                |> Array.unfold (fun q ->
                    match q.Length with
                    | 0 -> None
                    | _ ->
                        match q[0] with
                        | (v, (x, y), (dx, dy), sc) ->
                            match l[x][y] with
                            | 'E' -> Some(sc, q.Tail)
                            | _ ->
                                match sc with
                                | sc when sc > nl[x][y] -> Some(Int32.MaxValue, q.Tail)
                                | _ ->
                                    match (nl[x][y] <- sc) with
                                    | _ ->
                                        Some(
                                            Int32.MaxValue,
                                            q.Tail
                                            @ ([ (-1, 0); (1, 0); (0, -1); (0, 1) ]
                                               |> List.filter (fun (tx, ty) ->
                                                   match x + tx with
                                                   | x when x < 0 || x >= n -> false
                                                   | _ ->
                                                       match y + ty with
                                                       | y when y < 0 || y >= m -> false
                                                       | _ ->
                                                           match l[x + tx][y + ty] with
                                                           | 'E'
                                                           | '.' ->
                                                               sc <= nl[x + tx][y + ty]
                                                               && (not (v.Contains((x + tx, y + ty))))
                                                           | _ -> false)
                                               |> List.map (fun (tx, ty) ->
                                                   (v.Add((x, y)),
                                                    (x + tx, y + ty),
                                                    (tx, ty),
                                                    sc + 1001 - 1000 * abs (dx * tx + dy * ty))))

                                        ))
                |> Array.min


let ans2 =
    (lines, lines.Length, lines[0].Length)
    |||> fun l n m ->
        l
        |> Array.map Array.copy
        |> Array.mapi (fun i r ->
            r
            |> Array.mapi (fun j x ->
                match x with
                | 'S' -> i * n + j
                | _ -> 0)
            |> Array.sum)
        |> Array.sum
        |> fun x -> (x / n, x % n)
        ||> fun sx sy ->
            [| 0 .. (n - 1) |]
            |> Array.map (fun _ -> Array.create m Int32.MaxValue)
            |> fun nl ->
                ((Set.empty,
                  [ (Set.empty, (sx, sy), (0, 1), 0) ]
                  |> Array.unfold (fun q ->
                      match q.Length with
                      | 0 -> None
                      | _ ->
                          match q[0] with
                          | (v, (x, y), (dx, dy), sc) ->
                              match sc with
                              | sc when sc - 1001 > nl[x][y] -> Some((Int32.MaxValue, Set.empty), q.Tail)
                              | _ ->
                                  match (nl[x][y] <- sc) with
                                  | _ ->
                                      match l[x][y] with
                                      | 'E' -> Some((sc, v), q.Tail)
                                      | _ ->
                                          Some(
                                              (Int32.MaxValue, Set.empty),
                                              q.Tail
                                              @ ([ (-1, 0); (1, 0); (0, -1); (0, 1) ]
                                                 |> List.filter (fun (tx, ty) ->
                                                     match x + tx with
                                                     | x when x < 0 || x >= n -> false
                                                     | _ ->
                                                         match y + ty with
                                                         | y when y < 0 || y >= m -> false
                                                         | _ ->
                                                             match l[x + tx][y + ty] with
                                                             | 'E'
                                                             | '.' ->
                                                                 sc - 1001 <= nl[x + tx][y + ty]
                                                                 && (not (v.Contains((x + tx, y + ty))))
                                                             | _ -> false)
                                                 |> List.map (fun (tx, ty) ->
                                                     (v.Add((x, y)),
                                                      (x + tx, y + ty),
                                                      (tx, ty),
                                                      sc + 1001 - 1000 * abs (dx * tx + dy * ty))))

                                          ))
                  |> Array.groupBy fst
                  |> Array.minBy fst
                  |> snd)
                 ||> Array.fold (fun a (_, x) -> a |> Set.union x)
                 |> Set.count)
                + 1

stopWatch.Stop()
printfn "%A \n%A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
