open System
open System.IO
open System.Numerics

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllLines(@"input")
    |> Array.map (fun x -> x.Split ": ")
    |> Array.map (fun x ->
        (x[0] |> BigInteger.Parse, x[1].Split " " |> Array.map BigInteger.Parse |> Array.rev |> List.ofSeq))
    |> Array.mapi (fun i (a, b) -> (i, a, b, BigInteger(1)))

let ans1 =
    lines
    |> List.ofSeq
    |> Array.unfold (fun l ->
        match l.Length with
        | 0 -> None
        | _ ->
            match l[0] with
            | (i, t, n, c) ->
                match t with
                | t when t <= 0 -> Some(-1, l |> List.tail)
                | _ ->
                    match n.Length with
                    | 1 ->
                        match n[0] with
                        | x when t = x -> Some(i, l |> List.tail)
                        | _ -> Some(-1, l |> List.tail)
                    | _ ->
                        match n[0] with
                        | x when t % x = 0 ->
                            Some(
                                -1,
                                (i, t / x, n |> List.tail, c)
                                :: (i, t - x, (n |> List.tail), c)
                                :: (l |> List.tail)
                            )
                        | x -> Some(-1, (i, t - x, n |> List.tail, c) :: (l |> List.tail)))
    |> Array.filter ((<>) -1)
    |> Array.countBy id
    |> Array.sumBy (fun (i, c) ->
        match lines[i] with
        | (a, b, c, d) -> b)

let ans2 =
    lines
    |> List.ofSeq
    |> Array.unfold (fun l ->
        match l.Length with
        | 0 -> None
        | _ ->
            match l[0] with
            | (i, t, n, c) ->
                match t with
                | t when t <= 0 -> Some(-1, l |> List.tail)
                | _ ->
                    match n.Length with
                    | 1 ->
                        match t with
                        | x when x = n[0] * c -> Some(i, l |> List.tail)
                        | _ -> Some(-1, l |> List.tail)
                    | _ ->
                        match n[0] with
                        | x when t % x = 0 ->
                            Some(
                                -1,
                                (i, t / x, n |> List.tail, c)
                                :: (i, t - x * c, (n |> List.tail), c)
                                :: (i,
                                    t - x * c,
                                    (n |> List.tail),
                                    c * BigInteger.Pow(10, n[0] |> string |> String.length))
                                :: (l |> List.tail)
                            )
                        | x ->
                            Some(
                                -1,
                                (i, t - x * c, n |> List.tail, c)
                                :: (i,
                                    t - x * c,
                                    (n |> List.tail),
                                    c * BigInteger.Pow(10, n[0] |> string |> String.length))
                                :: (l |> List.tail)
                            ))
    |> Array.filter ((<>) -1)
    |> Array.countBy id
    |> Array.sumBy (fun (i, c) ->
        match lines[i] with
        | (a, b, c, d) -> b)

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
