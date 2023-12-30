#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "21.txt"
let size = (input |> Seq.length)
let halfSize = size / 2
let start = 0, 0
let map =
    ([ 0 .. size - 1 ], [ 0 .. size - 1 ])
    ||> List.allPairs
    |> List.fold
        (fun m (x, y) ->
            match input[y][x] with
            | '.' | 'S' -> m |> Set.add (x - halfSize, y - halfSize)
            | _ -> m)
        Set.empty

let gp (x, y) =
    let wrap v =
        let v = v % size
        if abs v > halfSize then v - (sign v) * size else v

    map.Contains(wrap x, wrap y)

let neighbours (x, y) =
    [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ] |> List.filter gp

let rec search maxSteps dist q =
    match q with
    | [] -> dist
    | p :: rest ->
        match (dist |> Map.find p) + 1 with
        | steps when steps > maxSteps ->
            search maxSteps dist rest
        | steps ->
            neighbours p
            |> List.fold
                (fun (dist, q) n ->
                    match dist |> Map.tryFind n |> Option.defaultValue 999999 with
                    | d when d > steps ->
                        dist |> Map.add n steps, q @ [ n ]
                    | _ -> dist, q)
                (dist, rest)
            ||> search maxSteps

let countInSlice result steps sx sy =
    result
    |> Map.filter (fun (x, y) d ->
        x >= sx * size - halfSize && x <= sx * size + halfSize &&
        y >= sy * size - halfSize && y <= sy * size + halfSize &&
        d % 2 = steps % 2)
    |> Seq.length

let steps = 5 * size + halfSize
let fromStart steps = search steps (Map [ start, 0 ]) [ start ]
let result = fromStart steps

for y in -10 .. 10 do
    for x in -10 .. 10 do
        let c = countInSlice result steps x y
        if c = 0 then
            printf "     "
        else
            printf "%5d" c
    printfn "\n"

[
    for y in -5 .. 5 do
        for x in -5 .. 5 do
            countInSlice result steps x y
] |> List.countBy id

let w = 202300L

let partTwo =
    5765L + 5755L + 5788L + 5732L +
    w * (980L + 1000L + 995L + 1000L) +
    (w - 1L) * (6676L + 6706L + 6709L + 6683L) +
    (pown w 2) * 7770L +
    (pown (w - 1L) 2) * 7627L
