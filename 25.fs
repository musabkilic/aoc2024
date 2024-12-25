open System
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllText(@"input").Trim().Split("\n\n")
    |> Array.map (fun x -> x.Split("\n") |> Array.map Array.ofSeq)

let ans1 =
    lines
    |> Array.partition (fun x -> x[0][0] = '#')
    ||> (fun a b ->
        (a
         |> Array.map (fun x ->
             [| 0..4 |]
             |> Array.map (fun i -> [| 1..6 |] |> Array.takeWhile (fun j -> x[j][i] = '#') |> Array.length)),
         b
         |> Array.map (fun x ->
             [| 0..4 |]
             |> Array.map (fun i -> [| 5..-1..0 |] |> Array.takeWhile (fun j -> x[j][i] = '#') |> Array.length))))
    ||> fun L K ->
        Array.allPairs L K
        |> Array.Parallel.filter (fun (l, k) -> [| 0..4 |] |> Array.forall (fun i -> l[i] + k[i] < 6))
        |> Array.length

stopWatch.Stop()
printfn "%A" ans1
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
