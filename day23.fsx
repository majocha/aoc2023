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

let rec longest steps visited moves =


    let ls = 
        [ for p in next move - visited do
            longest (steps + 1) (visited |> Set.add move) p
        ]
    match ls with | [] -> steps | ls -> ls |> List.max

let partOne = longest 0 Set.empty (1, 0)
