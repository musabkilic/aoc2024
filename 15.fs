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
    File.ReadAllText(@"input").Trim().Split("\n\n")
    |> fun x ->
        (x[0].Split("\n") |> Array.map (fun y -> y.ToCharArray()),
         x[1].Split("\n") |> String.concat "" |> (fun x -> x.ToCharArray()))

let ans1 =
    lines
    ||> fun pl di ->
        pl
        |> Array.map Array.copy
        |> fun l ->
            (l.Length, l[0].Length)
            ||> fun n m ->
                (List.allPairs [ 0 .. (n - 1) ] [ 0 .. (m - 1) ]
                 |> List.find (fun (x, y) -> l[x][y] = '@'),
                 di)
                ||> Array.fold (fun (rx, ry) d ->
                    match d with
                    | '^' ->
                        match
                            [ 0 .. (rx - 1) ]
                            |> List.rev
                            |> List.takeWhile (fun i -> l[i][ry] <> '#')
                            |> List.tryFind (fun i -> l[i][ry] = '.')
                        with
                        | Some i ->
                            match
                                ((l[i][ry] <- 'O') = ())
                                && ((l[rx - 1][ry] <- '@') = ())
                                && ((l[rx][ry] <- '.') = ())
                            with
                            | _ -> (rx - 1, ry)
                        | None -> (rx, ry)
                    | '>' ->
                        match
                            [ (ry + 1) .. (m - 1) ]
                            |> List.takeWhile (fun i -> l[rx][i] <> '#')
                            |> List.tryFind (fun i -> l[rx][i] = '.')
                        with
                        | Some i ->
                            match
                                ((l[rx][i] <- 'O') = ())
                                && ((l[rx][ry + 1] <- '@') = ())
                                && ((l[rx][ry] <- '.') = ())
                            with
                            | _ -> (rx, ry + 1)
                        | None -> (rx, ry)
                    | 'v' ->
                        match
                            [ (rx + 1) .. (n - 1) ]
                            |> List.takeWhile (fun i -> l[i][ry] <> '#')
                            |> List.tryFind (fun i -> l[i][ry] = '.')
                        with
                        | Some i ->
                            match
                                ((l[i][ry] <- 'O') = ())
                                && ((l[rx + 1][ry] <- '@') = ())
                                && ((l[rx][ry] <- '.') = ())
                            with
                            | _ -> (rx + 1, ry)
                        | None -> (rx, ry)
                    | '<' ->
                        match
                            [ 0 .. (ry - 1) ]
                            |> List.rev
                            |> List.takeWhile (fun i -> l[rx][i] <> '#')
                            |> List.tryFind (fun i -> l[rx][i] = '.')
                        with
                        | Some i ->
                            match
                                ((l[rx][i] <- 'O') = ())
                                && ((l[rx][ry - 1] <- '@') = ())
                                && ((l[rx][ry] <- '.') = ())
                            with
                            | _ -> (rx, ry - 1)
                        | None -> (rx, ry)
                    | _ -> (rx, ry))
                |> fun _ ->
                    l
                    |> Array.mapi (fun i r ->
                        r
                        |> Array.mapi (fun j x ->
                            match x with
                            | 'O' -> i * 100 + j
                            | _ -> 0)
                        |> Array.sum)
                    |> Array.sum

let ans2 =
    lines
    ||> fun pl di ->
        pl
        |> Array.map (fun r ->
            r
            |> Array.map (fun x ->
                match x with
                | '#' -> "##".ToCharArray()
                | 'O' -> "[]".ToCharArray()
                | '.' -> "..".ToCharArray()
                | '@' -> "@.".ToCharArray()
                | _ -> [||])
            |> Array.collect id)
        |> Array.map Array.copy
        |> fun l ->
            (l.Length, l[0].Length)
            ||> fun n m ->
                (List.allPairs [ 0 .. (n - 1) ] [ 0 .. (m - 1) ]
                 |> List.find (fun (x, y) -> l[x][y] = '@'),
                 di)
                ||> Array.fold (fun (rx, ry) d ->
                    (match d with
                     | '^' -> (-1, 0)
                     | '>' -> (0, 1)
                     | 'v' -> (1, 0)
                     | '<' -> (0, -1)
                     | _ -> (0, 0))
                    ||> fun dx dy ->
                        ([ (rx, ry) ], true)
                        |> Array.unfold (fun (q, c) ->
                            match c with
                            | false -> None
                            | _ ->
                                match q.Length with
                                | 0 -> None
                                | _ ->
                                    match q[0] with
                                    | (x, y) ->
                                        match l[x + dx][y + dy] with
                                        | '#' -> Some((-1, -1), ([], false))
                                        | '.' -> Some((x, y), (q.Tail, true))
                                        | ']' when dy = 0 ->
                                            Some((x, y), (q.Tail @ [ (x + dx, y + dy); (x + dx, y + dy - 1) ], true))
                                        | '[' when dy = 0 ->
                                            Some((x, y), (q.Tail @ [ (x + dx, y + dy); (x + dx, y + dy + 1) ], true))
                                        | _ -> Some((x, y), (q.Tail @ [ (x + dx, y + dy) ], true)))
                        |> fun tr ->
                            match tr |> Array.last with
                            | (-1, -1) -> (rx, ry)
                            | _ ->
                                tr
                                |> Array.distinct
                                |> Array.rev
                                |> Array.map (fun (x, y) ->
                                    ((l[x + dx][y + dy] <- l[x][y]) = ()) && ((l[x][y] <- '.') = ()))
                                |> fun _ -> l |> (fun _ -> (rx + dx, ry + dy)))
            |> fun _ ->
                l
                |> Array.mapi (fun i r ->
                    r
                    |> Array.mapi (fun j x ->
                        match x with
                        | '[' -> i * 100 + j
                        | _ -> 0)
                    |> Array.sum)
                |> Array.sum

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
