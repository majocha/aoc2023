#time "on"

type Dir =
    | Up
    | Down
    | Left
    | Right

let move (x, y) =
    function
    | Up -> x, y - 1
    | Down -> x, y + 1
    | Left -> x - 1, y
    | Right -> x + 1, y

let input = System.IO.File.ReadAllLines "16.txt"
let grid = input |> Seq.toList |> List.map List.ofSeq
let bounds = grid.Length - 1

let nextFeature start =
    let rec nextFeature' ps (p, dir) =
        let x, y = move p dir

        if x < 0 || x > bounds || y < 0 || y > bounds then
            ps, []
        else
            let ps = (x, y) :: ps

            match grid[y][x] with
            | '\\' ->
                ps,
                (dir
                 |> function
                     | Left -> Up
                     | Right -> Down
                     | Up -> Left
                     | Down -> Right)
                |> List.singleton
            | '/' ->
                ps,
                (dir
                 |> function
                     | Right -> Up
                     | Left -> Down
                     | Up -> Right
                     | Down -> Left)
                |> List.singleton
            | '|' when dir = Left || dir = Right -> ps, [ Up; Down ]
            | '-' when dir = Up || dir = Down -> ps, [ Left; Right ]
            | '|'
            | '-' -> ps, [ dir ]
            | _ -> nextFeature' ps ((x, y), dir)

    nextFeature' [] start

let allBeams start =
    let rec loop prevStarts visited starts =
        let fragments =
            [ for start in starts - prevStarts do
                  let ps, ds = nextFeature start

                  let starts =
                      match ps with
                      | from :: _ -> [ for d in ds -> from, d ]
                      | _ -> []

                  ps, starts ]

        let prevStarts = starts + prevStarts
        let visited = visited + (fragments |> Seq.collect fst |> Set.ofSeq)
        let starts = fragments |> Seq.collect snd |> Set

        if starts.IsEmpty then
            visited
        else
            loop prevStarts visited starts

    loop Set.empty Set.empty (Set.singleton start)

let energy start = allBeams start |> Set.count

let partOne = energy ((-1, 0), Right)

let allStarting =
    let edge = [ 0 .. bounds - 1 ]

    [| for i in edge do
           (i, -1), Down
           (i, bounds), Up
           (-1, i), Right
           (bounds, i), Left |]

let partTwo = allStarting |> Array.Parallel.map energy |> Array.Parallel.max
