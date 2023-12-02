#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "2.txt"

let game s =
    let n, s = sscanf "Game %d: %s" s
    n, [ for c in s |> String.split [ "; "; ", " ] -> sscanf "%d %s" c ]

let games = input |> map game

let possible (_, g) =
    g
    |> List.forall (function
        | n, "red" -> n <= 12
        | n, "green" -> n <= 13
        | n, "blue" -> n <= 14
        | _ -> false)

let partOne = games |> filter possible |> map fst |> sum


let minSet (_, g) =
    let folder state c =
        let r, g, b = state

        match c with
        | n, "red" when n > r -> n, g, b
        | n, "green" when n > g -> r, n, b
        | n, "blue" when n > b -> r, g, n
        | _ -> state

    g |> fold folder (0, 0, 0)

let power (r, g, b) = r * g * b

let partTwo = games |> map (minSet >> power) |> sum
