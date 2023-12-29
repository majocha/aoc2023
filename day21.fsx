#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "21.txt"
let size = (input |> Seq.length)
let d = size / 2
let start = 0, 0
let map =
    ([ 0 .. size - 1 ], [ 0 .. size - 1 ])
    ||> List.allPairs
    |> List.fold
        (fun m (x, y) ->
            match input[y][x] with
            | '.' | 'S' -> m |> Set.add (x - d, y - d)
            | _ -> m)
        Set.empty

let gp (x, y) =
    let wrap v =
        if (v + d) % size < 0 then (v + d) % size + size - d else (v + d) % size

    map.Contains(wrap x, wrap y)

let neighbours (x, y) =
    [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ] |> List.filter gp

let rec search stopCondition dist q =
    match q with
    | [] -> dist
    | p :: rest ->
        let current = (dist |> Map.find p) + 1

        neighbours p
        |> List.fold
            (fun (dist, q) n ->
                match dist |> Map.tryFind n |> Option.defaultValue 999999 with
                | d when d > current -> dist |> Map.add n current, (if stopCondition n then q else q @ [ n ])
                | _ -> dist, q)
            (dist, rest)
        ||> search stopCondition

let fourGrid (x, y) =
    x < - 5 * size || y < - 5 * size || x > 4 * size - 1 || y > 4 * size - 1


let printGrid charF d =
    for y in -d .. d do
        for x in -d .. d do
            charF (x, y) |> printf "%c"
        printfn ""


printGrid (fun p -> if gp p then '.'  else '#') size



let fromStart = search fourGrid (Map [ start, 0 ]) [ start ]

let countReachable steps start =
    fromStart
    |> Map.filter (fun _ d -> d % 2 = steps % 2 && d <= steps)
    |> Map.count

let partOne = countReachable 64 start

let slice sx sy =
    [ for x in sx * size .. (sx + 1) * size - 1 do
          for y in sy * size .. (sy + 1) * size - 1 do
              x, y ]

let oddInSlice sx sy map =
    [ for p in slice sx sy do
          if map |> Map.containsKey p && map[p] % 2 = 1 && map[p] <= 6 * 131 + 65 then
              true ]
    |> List.length

for y in -7 .. 5 do
    for x in -6 .. 5 do
        printf "[ %2d, %2d %4d ] " x y (oddInSlice x y fromStart)

    printfn ""


size

[ 262..392 ] |> List.length
131 * 3
262 / 131
