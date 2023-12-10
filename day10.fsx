#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines("10.txt")

let W = input[0] |> length
let H = input |> length

type Dir = North | South | West | East
type Pos = int * int
type Pipe = Dir * Dir

let getChar (x, y) = input[y % H][x % W]

let move (x, y) = function
    | North -> x, y - 1  | South -> x, y + 1 | West -> x - 1, y | East -> x + 1, y

let connection = function North -> South | West -> East | South -> North | East -> West

let start =
    let y = input |> Seq.findIndex (String.contains 'S')
    let x = input[y].IndexOf('S')
    x, y

let pipe = function
    | '|' ->  [North; South]
    | '-' ->  [East; West]
    | 'L' ->  [North; East]
    | 'J' ->  [North; West]
    | '7' ->  [South; West]
    | 'F' ->  [South; East]
    | _ -> []

let loopLength start =
    let rec follow n from dir =
        let next = move from dir
        let nextDir = getChar next |> pipe |> List.except [ connection dir ]
        match nextDir with
        | _ when next = start -> n
        | [ nextDir ] -> follow (n + 1) next nextDir
        | _ -> 0

    [North; South; West; East] |> map (follow 0 start) |> List.max

loopLength start / 2 + 1
