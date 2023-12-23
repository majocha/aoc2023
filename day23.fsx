#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "23.txt"
let size = input[0].Length
let tiles =
    Set [
        for y in 0 .. size - 1 do
            for x in 0 .. size - 1 do
                if input[y][x] <> '#' then x, y
    ]

let next (x,y) =
    let moves = 
        match input[y][x] with
        //| '>' -> [x + 1, y]
        //| '<' -> [x - 1, y]
        //| '^' -> [x, y - 1]
        //| 'v' -> [x, y + 1]
        | '#' -> []
        | _ -> [x + 1, y; x - 1, y; x, y - 1; x, y + 1]

        |> Set
    Set.intersect moves tiles

let finish = size - 2, size - 1

let rec walk map =
    function
    | [] -> map
    | ((pos :: _) as path) :: rest ->
        let currentScore = map |> Map.tryFind pos |> Option.defaultValue 0
        let score = path |> List.length
        if currentScore < score then
            [ for p in next pos do if path |> List.contains p |> not then p :: path ] @ rest
            |> walk (map.Add(pos, score))
        else
            walk map rest

let start = 1, 0
let m = walk Map.empty [[start]]
m[finish] - 1