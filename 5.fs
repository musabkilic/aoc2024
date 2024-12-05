open System
open System.IO
open System.Text.RegularExpressions

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllText(@"input").Trim().Split("\n\n")
    |> fun x ->
        (x.[0], x.[1])
        ||> fun a b ->
            (a.Split "\n"
             |> Array.map (fun x -> x.Split "|" |> Array.map int)
             |> Array.countBy id
             |> Map.ofSeq,
             b.Split "\n" |> Array.map (fun x -> x.Split "," |> Array.map int))


let ans1 =
    lines
    ||> fun A B ->
        B
        |> Array.sumBy (fun l ->
            l
            |> Array.mapi (fun i x ->
                [| 0 .. (i - 1) |]
                |> Array.forall (fun j ->
                    match (A |> Map.tryFind [| l.[i]; l.[j] |] |> Option.defaultValue 0) with
                    | 1 -> false
                    | _ -> true))
            |> Array.forall id
            |> (fun x ->
                match x with
                | true -> l[(l |> Array.length) / 2]
                | _ -> 0))

let ans2 =
    (lines
     ||> fun A B ->
         B
         |> Array.sumBy (fun l ->
             l
             |> Array.mapi (fun i x ->
                 [| 0 .. (i - 1) |]
                 |> Array.forall (fun j ->
                     match (A |> Map.tryFind [| l.[i]; l.[j] |] |> Option.defaultValue 0) with
                     | 1 ->
                         ((l.[i] <- l.[i] + l.[j]) = ())
                         && ((l.[j] <- l.[i] - l.[j]) = ())
                         && ((l.[i] <- l.[i] - l.[j]) = ())
                     | _ -> true))
             |> Array.forall id
             |> (fun x ->
                 match x with
                 | true -> l[(l |> Array.length) / 2]
                 | _ -> 0)))
    - ans1

printfn "%A %A" ans1 ans2
