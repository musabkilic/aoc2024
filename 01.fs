open System
open System.IO

let DEBUG = false

let dbg v =
    if DEBUG then
        printfn "%A" v

    v

let lines =
    File.ReadAllLines(@"input")
    |> Array.map (fun x -> x.Split("   ") |> Array.map (int) |> (fun x -> (x.[0], x.[1])))
    |> Array.unzip
    |> fun (a, b) -> [| a; b |]

let ans1 =
    lines
    |> Array.map (Array.sort)
    |> fun x -> (x.[0], x.[1]) ||> Array.map2 (fun a b -> abs (a - b)) |> Array.sum

let ans2 =
    lines
    |> Array.map (Array.sort)
    |> fun x -> (x.[0], x.[1] |> Array.countBy (id) |> Map.ofArray)
    ||> fun A B -> A |> Array.sumBy (fun a -> a * (B |> Map.tryFind a |> Option.defaultValue 0))

printfn "%A %A" ans1 ans2
