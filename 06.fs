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
    |> Array.map Array.ofSeq
    |> Array.map (
        Array.map (fun x ->
            match x with
            | '#' -> 1
            | '>' -> 4
            | '^' -> 8
            | '<' -> 2
            | 'v' -> 16
            | _ -> 0)
    )

let ans1 =
    lines
    |> Array.map Array.copy
    |> (fun l ->
        (((l |> Array.length, l[0].Length, 0, 0, 0, 0),
          l
          |> Array.mapi (fun i r ->
              ((0, 0, 0, 0, 0, 0),
               r
               |> Array.mapi (fun j x ->
                   match x with
                   | 4 -> (0, 0, i, j, 0, 1)
                   | 8 -> (0, 0, i, j, -1, 0)
                   | 2 -> (0, 0, i, j, 0, -1)
                   | 16 -> (0, 0, i, j, 1, 0)
                   | _ -> (0, 0, 0, 0, 0, 0)))
              ||> Array.fold (fun (a, b, c, d, e, f) (x, y, z, t, u, v) -> (a + x, b + y, c + z, d + t, e + u, f + v))))
         ||> Array.fold (fun (a, b, c, d, e, f) (x, y, z, t, u, v) -> (a + x, b + y, c + z, d + t, e + u, f + v))
         |> Array.unfold (fun (n, m, x, y, i, j) ->
             match x with
             | x when x + i < 0 -> None
             | x when x + i >= n -> None
             | _ ->
                 match y with
                 | y when y + j < 0 -> None
                 | y when y + j >= m -> None
                 | _ ->
                     match l[x + i][y + j] with
                     | 1 -> Some(false, (n, m, x, y, j, -i))
                     | _ -> Some(((l[x][y] <- 2) = ()) && ((l[x + i][y + j] <- 2) = ()), (n, m, x + i, y + j, i, j)))
         |> fun t ->
             l
             |> Array.sumBy (fun r ->
                 r
                 |> Array.sumBy (fun x ->
                     match x with
                     | 1 -> 0
                     | 0 -> 0
                     | _ -> 1))))

let ans2 =
    lines
    |> Array.map Array.copy
    |> (fun l ->
        (((l |> Array.length, l[0].Length, 0, 0, 0, 0),
          l
          |> Array.mapi (fun i r ->
              ((0, 0, 0, 0, 0, 0),
               r
               |> Array.mapi (fun j x ->
                   match x with
                   | 4 -> (0, 0, i, j, 0, 1)
                   | 8 -> (0, 0, i, j, -1, 0)
                   | 2 -> (0, 0, i, j, 0, -1)
                   | 16 -> (0, 0, i, j, 1, 0)
                   | _ -> (0, 0, 0, 0, 0, 0)))
              ||> Array.fold (fun (a, b, c, d, e, f) (x, y, z, t, u, v) -> (a + x, b + y, c + z, d + t, e + u, f + v))))
         ||> Array.fold (fun (a, b, c, d, e, f) (x, y, z, t, u, v) -> (a + x, b + y, c + z, d + t, e + u, f + v))
         |> Array.unfold (fun (n, m, x, y, i, j) ->
             match x with
             | x when x + i < 0 -> None
             | x when x + i >= n -> None
             | _ ->
                 match y with
                 | y when y + j < 0 -> None
                 | y when y + j >= m -> None
                 | _ ->
                     match l[x + i][y + j] with
                     | 1 -> Some((-1, -1), (n, m, x, y, j, -i))
                     | 32 -> Some((-1, -1), (n, m, x + i, y + j, i, j))
                     | _ ->
                         match l[x][y] <- 32 with
                         | _ ->
                             l
                             |> Array.map Array.copy
                             |> fun L ->
                                 match L[x + i][y + j] <- 1 with
                                 | _ ->
                                     Some(
                                         match
                                             ((x, y, i, j)
                                              |> Array.unfold (fun (X, Y, I, J) ->
                                                  match X with
                                                  | X when X + I < 0 -> None
                                                  | X when X + I >= n -> None
                                                  | _ ->
                                                      match Y with
                                                      | Y when Y + J < 0 -> None
                                                      | Y when Y + J >= m -> None
                                                      | _ ->
                                                          match L[X + I][Y + J] with
                                                          | 1 -> Some(0, (X, Y, J, -I))
                                                          | _ ->
                                                              match L[X][Y] with
                                                              | a when
                                                                  (a &&& (4 * (I + 3 * abs (I)) + (J + 3 * abs (J)))) > 0
                                                                  ->
                                                                  Some(1, (n, n, n, n))
                                                              | _ ->
                                                                  Some(
                                                                      match
                                                                          L[X][Y] <-
                                                                              L[X][Y]
                                                                              ||| (4 * (I + 3 * abs (I))
                                                                                   + (J + 3 * abs (J)))
                                                                      with
                                                                      | _ -> 0, (X + I, Y + J, I, J)
                                                                  ))
                                              |> Array.sum)
                                         with
                                         | 1 -> (x + i, y + j)
                                         | _ -> (-1, -1)
                                         , (n, m, x + i, y + j, i, j)
                                     ))))
    |> Array.filter (fun (x, y) -> (x, y) <> (-1, -1) && lines[x][y] = 0)
    |> Array.countBy id
    |> Array.length

stopWatch.Stop()
printfn "%A %A" ans1 ans2
printfn "Time: %fms" stopWatch.Elapsed.TotalMilliseconds