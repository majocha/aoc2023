#time "on"

let input = System.IO.File.ReadAllLines "23.txt"

let size = input[0].Length

let tiles =
    Set [
        for y in 0 .. size - 1 do
            for x in 0 .. size - 1 do
                if input[y][x] <> '#' then x, y
    ]

let nextPartOne (x,y) =
    let moves = 
        match input[y][x] with
        | '>' -> [x + 1, y]
        | '<' -> [x - 1, y]
        | '^' -> [x, y - 1]
        | 'v' -> [x, y + 1]
        | '.' -> [x + 1, y; x - 1, y; x, y - 1; x, y + 1]
        | _ -> []
        |> Set
    Set.intersect moves tiles

let nextPartTwo (x,y) =
    let moves = 
        match input[y][x] with
        | '#' -> []
        | _ -> [x + 1, y; x - 1, y; x, y - 1; x, y + 1]
        |> Set
    Set.intersect moves tiles

let start = 1, 0
let finish = size - 2, size - 1

// Get a map of junctions and their distances.
let rec walkJunction next steps from prev map pos =
    let addJ p = map |> Map.add from ( (p, steps) :: map[from])
    let move = next pos |> Set.remove prev
    match move |> Set.toList with
    | [] -> map
    | [ move ] when move = finish  -> addJ finish
    | [ move ] -> walkJunction next (steps + 1) from pos map move
    | moves ->
        if map |> Map.containsKey pos then
            map |> Map.add pos ((from, steps) :: map[pos])
        else
            moves |> List.fold (walkJunction next 1 pos pos) (addJ pos |> Map.add pos [from, steps])

let walkJunctions next = walkJunction next 1 start start (Map [start, []]) start

// Just try all paths favoring longer, takes about 10 - 15 seconds.
let rec walk map js steps from =
    [ 
        for j, (d: int) in
            map |> Map.find from
            |> List.filter (fun (j, _) -> js |> List.contains j |> not)
            |> List.sortByDescending snd
            do
                let steps = steps + d
                if js |> List.contains j |> not then
                    if j = finish then
                        yield steps
                    else
                        yield! walk map (j :: js) steps j
    ]

let result map = walk map [] 0 start |> List.max

let partOne = walkJunctions nextPartOne |> result
let partTwo = walkJunctions nextPartTwo |> result
