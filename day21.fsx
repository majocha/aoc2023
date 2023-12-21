#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "21.txt"
let size = (input |> Seq.length)

let start, map =
    ( [ 0 .. size - 1 ], [0 .. size - 1 ] )
    ||> List.allPairs
    |> List.fold ( fun (s, m) (x, y) -> 
        match input[y][x] with
        | '.' -> s, m |> Set.add (x, y)
        | 'S' -> (x, y), m |> Set.add (x, y)
        | _ -> s, m )
        ((0,0), Set.empty)

let neighbours (x, y) =
    [
        for p in  [ x - 1, y; x + 1, y; x, y - 1; x, y + 1] do
            if map.Contains p then p ]
            // if map.Contains ((size + x) % size, (size + y) % size) then x, y ]

let distances start =
    let rec search n dist q =
        match q with
        | [] -> dist
        | p :: rest ->
            match dist |> Map.tryFind p |> Option.defaultValue 999 with
            | d when n < d ->
                search
                    (n + 1)
                    (dist |> Map.add p n)
                    (q @ neighbours p)
            | _ -> search n dist rest
    search 0 Map.empty [ start ]

let ds = distances start


