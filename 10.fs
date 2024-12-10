open System
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllLines(@"input")
    |> Array.map Array.ofSeq
    |> Array.map (Array.map (fun x -> x |> Char.GetNumericValue |> int))

let ans1 =
    (lines, lines.Length, lines[0].Length)
    |||> (fun l n m ->
        l
        |> Array.mapi (fun i r ->
            r
            |> Array.mapi (fun j v ->
                match v with
                | 0 -> (i, j)
                | _ -> (-1, -1)))
        |> Array.collect id
        |> Array.filter ((<>) (-1, -1))
        |> Array.sumBy (fun (X, Y) ->
            [ (X, Y, 0) ]
            |> Array.unfold (fun s ->
                match s.Length with
                | 0 -> None
                | _ ->
                    match s[0] with
                    | (x, y, 9) -> Some((x, y), List.tail s)
                    | (x, y, i) ->
                        Some(
                            (-1, -1),
                            ([ (x + 1, y, i + 1); (x - 1, y, i + 1); (x, y + 1, i + 1); (x, y - 1, i + 1) ]
                             |> List.filter (fun (x, y, i) ->
                                 match x with
                                 | x when x < 0 || x >= n -> false
                                 | _ ->
                                     match y with
                                     | y when y < 0 || y >= m -> false
                                     | _ ->
                                         match l[x][y] with
                                         | j when j = i -> true
                                         | _ -> false))
                            @ (List.tail s)
                        ))
            |> Array.distinct
            |> Array.filter ((<>) (-1, -1))
            |> Array.length))

let ans2 =
    (lines, lines.Length, lines[0].Length)
    |||> (fun l n m ->
        l
        |> Array.mapi (fun i r ->
            r
            |> Array.mapi (fun j v ->
                match v with
                | 0 -> (i, j)
                | _ -> (-1, -1)))
        |> Array.collect id
        |> Array.filter ((<>) (-1, -1))
        |> Array.sumBy (fun (X, Y) ->
            [ (X, Y, 0) ]
            |> Array.unfold (fun s ->
                match s.Length with
                | 0 -> None
                | _ ->
                    match s[0] with
                    | (x, y, 9) -> Some(1, List.tail s)
                    | (x, y, i) ->
                        Some(
                            0,
                            ([ (x + 1, y, i + 1); (x - 1, y, i + 1); (x, y + 1, i + 1); (x, y - 1, i + 1) ]
                             |> List.filter (fun (x, y, i) ->
                                 match x with
                                 | x when x < 0 || x >= n -> false
                                 | _ ->
                                     match y with
                                     | y when y < 0 || y >= m -> false
                                     | _ ->
                                         match l[x][y] with
                                         | j when j = i -> true
                                         | _ -> false))
                            @ (List.tail s)
                        ))
            |> Array.sum))

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
