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
    |> fun x -> (x[0].Trim().Split(", "), x[1].Split("\n"))

let ans1 =
    lines
    ||> fun d l ->
        l
        |> Array.filter (fun w ->
            [| 0 .. (w.Length) |]
            |> Array.map ((=) 0)
            |> fun r ->
                r
                |> Array.iteri (fun i v ->
                    match v with
                    | false -> ()
                    | true ->
                        d
                        |> Array.filter (fun c -> w.Substring(i).StartsWith(c))
                        |> Array.iter (fun c -> r[i + c.Length] <- true))
                |> fun _ -> r |> Array.last)
        |> Array.length

let ans2 =
    lines
    ||> fun d l ->
        l
        |> Array.Parallel.map (fun w ->
            [| 0 .. (w.Length) |]
            |> Array.map (fun i -> if i = 0 then 1I else 0I)
            |> fun r ->
                r
                |> Array.iteri (fun i v ->
                    match v with
                    | v when v = 0I -> ()
                    | _ ->
                        d
                        |> Array.filter (fun c -> w.Substring(i).StartsWith(c))
                        |> Array.iter (fun c -> r[i + c.Length] <- r[i + c.Length] + v))
                |> fun _ -> r |> Array.last)
        |> Array.sum

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
