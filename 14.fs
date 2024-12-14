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
    File.ReadAllLines(@"input")
    |> Array.map (fun x ->
        x
        |> Regex("-?\\d+").Matches
        |> Array.ofSeq
        |> Array.map (fun x -> x |> string |> int)
        |> (fun x -> (x[0], x[1], x[2], x[3])))

let ans1 =
    ((0, 0, 0, 0),
     lines
     |> Array.map (fun (px, py, vx, vy) ->
         match (((px + vx * 100) % 101) + 101) % 101, (((py + vy * 100) % 103) + 103) % 103 with
         | (x, y) ->
             ((x < 50 && y < 51) |> Convert.ToInt32,
              (x > 50 && y < 51) |> Convert.ToInt32,
              (x < 50 && y > 51) |> Convert.ToInt32,
              (x > 50 && y > 51) |> Convert.ToInt32)))
    ||> Array.fold (fun (a1, a2, a3, a4) (x1, x2, x3, x4) -> (a1 + x1, a2 + x2, a3 + x3, a4 + x4))
    |> fun (q1, q2, q3, q4) -> q1 * q2 * q3 * q4

let ans2 =
    1
    |> Array.unfold (fun k ->
        lines
        |> Array.map (fun (px, py, vx, vy) ->
            ((((px + vx * k) % 101) + 101) % 101, (((py + vy * k) % 103) + 103) % 103), 1)
        |> Map
        |> fun m ->
            [| 0..103 |]
            |> Array.map (fun i ->
                [| 0..101 |]
                |> Array.map (fun j ->
                    match m.ContainsKey(j, i) with
                    | true -> '*'
                    | false -> ' ')
                |> String)
            |> String.concat "\n"
        |> fun x -> printfn "\n\n\n\n\n\n\n\n%s" x
        |> fun _ ->
            match Console.ReadLine() with
            | "y" -> None
            | "b" -> Some(k - 1, k - 1)
            | _ -> Some(k + 1, k + 1))
    |> Array.last

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
