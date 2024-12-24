open System
open System.IO
open System.Collections.Generic

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let DEBUG = true

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllText(@"input").Trim().Split("\n\n")
    |> fun x ->
        (x[0].Split("\n")
         |> Array.map (fun y -> y.Split(": ") |> fun z -> (z[0], z[1] |> int)),
         x[1].Split("\n")
         |> Array.map (fun y ->
             y.Split(" -> ")
             |> fun z -> (z[0].Split(" ") |> fun t -> (t[0], t[1], t[2]), z[1])))

let ans1 =
    lines
    |> fun (a, b) ->
        (a |> dict |> Dictionary, b)
        ||> fun v c ->
            [ 0 .. (c.Length - 1) ]
            |> Array.unfold (fun q ->
                match q.Length with
                | 0 -> None
                | _ ->
                    Some(
                        0,
                        q
                        |> List.filter (fun i ->
                            match c[i] with
                            | ((a, g, b), t) ->
                                match v.ContainsKey a && v.ContainsKey b with
                                | true ->
                                    match g with
                                    | "AND" -> v.Add(t, v[a] &&& v[b]) <> ()
                                    | "OR" -> v.Add(t, v[a] ||| v[b]) <> ()
                                    | _ -> v.Add(t, v[a] ^^^ v[b]) <> ()
                                | false -> true)
                    ))
            |> fun _ ->
                0
                |> Array.unfold (fun i ->
                    match "z" + (i |> string).PadLeft(2, '0') with
                    | z when v.ContainsKey z -> Some(v[z] |> int64, i + 1)
                    | _ -> None)
                |> Array.mapi (fun i x -> x <<< i)
                |> Array.sum

let ans2 =
    lines
    |> fun (a, b) ->
        (a |> dict |> Dictionary, b)
        ||> fun v c ->
            (0
             |> Array.unfold (fun i ->
                 match "x" + (i |> string).PadLeft(2, '0') with
                 | x when v.ContainsKey x -> Some(v[x], i + 1)
                 | _ -> None),
             0
             |> Array.unfold (fun i ->
                 match "y" + (i |> string).PadLeft(2, '0') with
                 | x when v.ContainsKey x -> Some(v[x], i + 1)
                 | _ -> None))
            ||> fun x y ->
                (0, [ 0 .. (x.Length - 1) ])
                ||> List.mapFold (fun c i -> (x[i] ^^^ y[i] ^^^ c, (x[i] &&& y[i]) ||| (x[i] &&& c) ||| (y[i] &&& c)))
                ||> fun a b -> a @ [ b ]
                |> List.mapi (fun i x -> ("z" + (i |> string).PadLeft(2, '0'), x))
                |> Map
            |> fun M ->
                (0, [ 0 .. (c.Length - 1) ], Dictionary<string, (string * string * string)>())
                |> Array.unfold (fun (k, q, s) ->
                    match q.Length with
                    | 0 -> None
                    | _ ->
                        match
                            q
                            |> List.filter (fun i ->
                                match c[i] with
                                | ((a, g, b), t) ->
                                    match
                                        (((not (a.StartsWith("x"))) && (not (a.StartsWith("y"))))
                                         || (a.Substring(1) |> int) <= k)
                                        && (((not (a.StartsWith("x"))) && (not (a.StartsWith("y"))))
                                            || (b.Substring(1) |> int) <= k)
                                        && v.ContainsKey a
                                        && v.ContainsKey b
                                    with
                                    | true ->
                                        match
                                            match g with
                                            | "AND" -> v[a] &&& v[b]
                                            | "OR" -> v[a] ||| v[b]
                                            | _ -> v[a] ^^^ v[b]
                                        with
                                        | r ->
                                            match t.StartsWith("z") && r <> M[t] with
                                            | true ->
                                                v.Add(t, r) <> s.Add(t, (a, g, b))
                                                |> fun _ ->
                                                    ("\n\n\n\n\n\n\n\n\n", (a, g, b), t, s) |> dbg |> (fun _ -> false)
                                            | false ->
                                                v.Add(t, r) <> s.Add(t, (a, g, b))
                                                |> fun _ -> ((a, g, b), t, s) |> dbg |> (fun _ -> false)
                                    | false -> true)
                        with
                        | l when l.Length = q.Length ->
                            Some(0, (k + 1, q, Dictionary<string, (string * string * string)>()))
                        | l -> Some(0, (k, l, s)))


stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds
