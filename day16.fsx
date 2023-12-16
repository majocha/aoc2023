#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

type Dir = Up | Down | Left | Right

let move (x, y) =
    function
    | Up -> x, y - 1
    | Down -> x, y + 1
    | Left -> x - 1, y
    | Right -> x + 1, y

let input = System.IO.File.ReadAllLines "16.txt"
let grid = input |> Seq.toList |> List.map List.ofSeq
let bounds = grid.Length - 1

let (|Inside|_|) (x, y) = 
    if x < 0 || x > bounds || y < 0 || y > bounds then None else Some(x, y)

let getTile = function Inside(x, y) -> Some (grid[y][x]) | _ -> None

let (|Reflect|_|) dir c =
    match c with
    | Some '\\' -> (dir |> function Left -> Up | Right -> Down | Up -> Left | Down -> Right) |> List.singleton |> Some
    | Some '/' -> (dir |> function Right -> Up | Left -> Down | Up -> Right | Down -> Left) |> List.singleton |> Some
    | _ -> None

let (|Split|_|) dir c =
    match c with
    | Some '|' -> (dir |> function  Left | Right -> [Up; Down] | d -> [ d ] ) |> Some
    | Some '-' -> (dir |> function  Up | Down -> [Left; Right] | d -> [ d ] ) |> Some
    | _ -> None

let step from dir =
    let next = move from dir 
    match next |> getTile with
    | Reflect dir dirs -> [for d in dirs -> next, d]
    | Split dir dirs -> [for d in dirs -> next, d]
    | Some _ -> [ next, dir ]
    | _ -> []

let rec step' acc prev =
    if prev |> Set.isEmpty then acc
    else
        let next =
            [ for from, dir in prev do
                yield! step from dir
            ] |> Set
        step' (acc + next) (next - acc)

let initial = step (-1, 0) Right |> Set
let partOne = step' initial initial |> Set.map fst |> Set.count
    
    


