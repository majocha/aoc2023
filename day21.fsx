#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "21.txt"
let size = (input |> Seq.length) - 1

let start, map =
    ( [ 0 .. size ], [0 .. size ] )
    ||> List.allPairs
    |> List.fold ( fun (s, m) (x, y) -> 
        match input[y][x] with
        | '.' -> s, m |> Set.add (x, y)
        | 'S' -> (x, y), m |> Set.add (x, y)
        | _ -> s, m )
        ((0,0), Set.empty)

let from =
    fun (x,y) -> Set [ x - 1, y; x + 1, y; x, y - 1; x, y + 1] |> Set.intersect map
    |> memoizeN

let rec walk' =
    fun n p -> if n = 0 then Set.singleton p else from p |> Set.map (walk' (n - 1)) |> Set.unionMany
    |> memoizeN

let walk n = walk' n start

walk 64 |> Set.count