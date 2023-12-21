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

let gp (x, y) =
    let wrap v = if v % size < 0 then v % size + size else v % size
    map.Contains (wrap x, wrap y)

let neighbours (x, y) = [ x - 1, y; x + 1, y; x, y - 1; x, y + 1] |> List.filter gp

let reachable steps =
    let rec search dist q =
        match q with
        | [] -> dist
        | p :: rest ->
            let current = (dist |> Map.find p) + 1
            neighbours p
            |> List.fold ( fun (dist, q) n ->
                match dist |> Map.tryFind n |> Option.defaultValue 999999 with
                | d when d > current ->
                    dist |> Map.add n current,
                    if current = steps then q else n :: q
                | _ -> dist, q )
                (dist, rest)
            ||> search

    search (Map [start, 0]) [ start ]
    |> Map.filter (fun _ d -> d % 2 = steps % 2 && d <= steps) |> Map.count

let partOne = reachable 64


