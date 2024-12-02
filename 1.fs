open System
open System.IO

let lines =
    File.ReadAllLines(@"1.txt")
    |> Array.map (fun x -> x.Split("   ") |> Array.map (fun x -> x |> int))

let A = lines |> Array.map (fun x -> x[0]) |> Array.sort
let B = lines |> Array.map (fun x -> x[1]) |> Array.sort
let ans1 = (A, B) ||> Array.map2 (fun a b -> abs (a - b)) |> Array.sum

let ans2 =
    A
    |> Array.map (fun a -> a * (B |> Array.filter (fun x -> x = a) |> Array.length))
    |> Array.sum

printfn "%i %i" ans1 ans2
