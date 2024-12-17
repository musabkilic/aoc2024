open System
open System.IO
open System.Numerics
open System.Text.RegularExpressions

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = true

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllText(@"input").Trim().Split("\n\n")
    |> Array.map (Regex("\\d+").Matches)
    |> Array.map Array.ofSeq
    |> Array.map (Array.map (fun x -> x |> string |> int))
    |> fun x -> ((x[0][0], x[0][1], x[0][2]), x[1])

let ans1 =
    lines
    ||> fun (A, B, C) p ->
        (0, A, B, C)
        |> Array.unfold (fun (i, A, B, C) ->
            match i with
            | i when i >= p.Length -> None
            | _ ->
                match (p[i], p[i + 1]) with
                | (op, opl) ->
                    match
                        match opl with
                        | 0
                        | 1
                        | 2
                        | 3 -> opl
                        | 4 -> A
                        | 5 -> B
                        | 6 -> C
                        | _ -> 0
                    with
                    | opc ->
                        match op with
                        | 0 -> Some(-1, (i + 2, A >>> opc, B, C))
                        | 1 -> Some(-1, (i + 2, A, B ^^^ opl, C))
                        | 2 -> Some(-1, (i + 2, A, opc &&& 7, C))
                        | 3 ->
                            match A with
                            | 0 -> Some(-1, (i + 2, A, B, C))
                            | _ -> Some(-1, (opl, A, B, C))
                        | 4 -> Some(-1, (i + 2, A, B ^^^ C, C))
                        | 5 -> Some(opc &&& 7, (i + 2, A, B, C))
                        | 6 -> Some(-1, (i + 2, A, A >>> opc, C))
                        | 7 -> Some(-1, (i + 2, A, B, A >>> opc))
                        | _ -> Some(666, (666, 666, 666, 666)))
    |> Array.filter ((<>) -1)
    |> Array.map string
    |> String.concat ","

let ans2 =
    lines
    ||> fun _ p ->
        [| 0 .. (p.Length + 2) |]
        |> Array.map (fun _ -> 0)
        |> fun r ->
            (0, true)
            |> Array.unfold (fun (i, w) ->
                match i with
                | i when i = p.Length -> None
                | _ ->
                    match
                        (match w with
                         | true -> [ 0..7 ]
                         | false -> [ (r[i + 3] + 1) .. 7 ])
                        |> List.tryFind (fun d ->
                            match (r[i] <<< 9) + (r[i + 1] <<< 6) + (r[i + 2] <<< 3) + d with
                            | A ->
                                match ((A &&& 7) ^^^ 1) with
                                | B ->
                                    match A >>> B with
                                    | C -> (((B ^^^ 5) ^^^ C) &&& 7) = p[p.Length - 1 - i])
                    with
                    | Some x ->
                        match r[i + 3] <- x with
                        | _ -> Some(None, (i + 1, true))
                    | None -> Some(None, (i - 1, false)))
            |> fun _ -> r |> Array.rev |> Array.mapi (fun i x -> (x |> bigint) <<< (i * 3)) |> Array.sum

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
