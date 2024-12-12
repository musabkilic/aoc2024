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
    File.ReadAllText(@"input").Trim()
    |> Array.ofSeq
    |> Array.map (fun x -> (x |> int) - 48)

let ans1 =
    lines
    |> Array.mapi (fun i x ->
        match i with
        | i when i % 2 = 0 -> Array.create x (i / 2)
        | _ -> Array.create x -1)
    |> Array.collect id
    |> fun a ->
        ((0, a.Length - 1)
         |> Array.unfold (fun (i, j) ->
             match i with
             | i when i >= j -> None
             | i when a[i] <> -1 -> Some(i, (i + 1, j))
             | _ ->
                 match j with
                 | j when a[j] = -1 -> Some(i - 1, (i, j - 1))
                 | _ ->
                     match
                         ((a[i] <- a[i] + a[j]) = ())
                         && ((a[j] <- a[i] - a[j]) = ())
                         && ((a[i] <- a[i] - a[j]) = ())
                     with
                     | _ -> Some(i, (i + 1, j - 1)))
         |> fun _ -> a |> Array.filter ((<>) -1) |> Array.map bigint)
        |> Array.mapi (fun i x -> (i |> bigint) * x)
        |> Array.sum


let ans2 =
    (lines |> Array.mapi (fun i x -> (x, i % 2 = 0)), lines.Length)
    ||> fun a n ->
        0
        |> Array.unfold (fun i ->
            match i with
            | i when i = n -> None
            | i when snd a[i] -> Some(Array.create (fst a[i]) (i / 2), i + 1)
            | _ ->
                match
                    [| (i + 1) .. (n - 1) |]
                    |> Array.rev
                    |> Array.tryFind (fun j ->
                        match j with
                        | j when snd a[j] && fst a[j] <= fst a[i] -> true
                        | _ -> false)
                with
                | Some j ->
                    match a[j] with
                    | (t, _) ->
                        match ((a[i] <- ((fst a[i]) - t, (fst a[i]) = t)) = ()) && ((a[j] <- (t, false)) = ()) with
                        | _ -> Some(Array.create t (j / 2), i)
                | None -> Some(Array.create (fst a[i]) -1, i + 1))
        |> Array.collect id
        |> Array.mapi (fun i x ->
            match x with
            | -1 -> BigInteger(0)
            | _ -> (i |> bigint) * (x |> bigint))
        |> Array.sum

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
