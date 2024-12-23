open System
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllText(@"input").Trim().Split("\n")
    |> Array.map (fun x -> x.Split("-") |> fun i -> (i[0], i[1]))

let ans1 =
    Array.append lines (lines |> Array.map (fun x -> (snd x, fst x)))
    |> Array.groupBy fst
    |> Array.map (fun (a, b) -> (a, b |> Array.map snd |> Set.ofSeq))
    |> Map
    |> fun M ->
        M
        |> Map.keys
        |> Seq.filter (fun x -> x.StartsWith("t"))
        |> Array.ofSeq
        |> Array.map (fun x ->
            M[x]
            |> fun m ->
                m
                |> Set.map (fun y -> M[y] |> Set.intersect m |> Set.map (fun z -> [ x; y; z ] |> Set))
                |> Set.unionMany)
        |> Set.unionMany
        |> Set.count

let ans2 =
    Array.append lines (lines |> Array.map (fun x -> (snd x, fst x)))
    |> Array.groupBy fst
    |> Array.map (fun (a, b) -> (a, b |> Array.map snd |> Set.ofSeq))
    |> Map
    |> fun M ->
        M
        |> Map.keys
        |> Array.ofSeq
        |> Array.map (fun x ->
            [ (Set.singleton x, M[x]) ]
            |> Array.unfold (fun q ->
                match q.Length with
                | 0 -> None
                | _ ->
                    match q[0] with
                    | (n, c) ->
                        match c.Count with
                        | 0 -> Some(n, q.Tail)
                        | _ ->
                            match c |> Set.minElement with
                            | fc -> Some(Set.empty, [ (n.Add(fc), c.Remove(fc) |> Set.intersect (M[fc])) ] @ q.Tail))
            |> Array.maxBy Set.count)
    |> Array.maxBy Set.count
    |> Array.ofSeq
    |> Array.sort
    |> String.concat ","

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
