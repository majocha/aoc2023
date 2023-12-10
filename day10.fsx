#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines("10.txt")

let W = input[0].Length
let H = input.Length

type Dir =
    | North
    | South
    | West
    | East

type Pos = int * int
type Pipe = Dir * Dir

let getChar (x, y) =
    if x >= 0 && x < W && y >= 0 && y < H then
        input[y][x]
    else
        ' '

let move (x, y) =
    function
    | North -> x, y - 1
    | South -> x, y + 1
    | West -> x - 1, y
    | East -> x + 1, y

let connection =
    function
    | North -> South
    | West -> East
    | South -> North
    | East -> West

let start =
    let y = input |> Seq.findIndex (String.contains 'S')
    let x = input[y].IndexOf('S')
    x, y

let pipe =
    function
    | '|' -> [ North; South ]
    | '-' -> [ East; West ]
    | 'L' -> [ North; East ]
    | 'J' -> [ North; West ]
    | '7' -> [ South; West ]
    | 'F' -> [ South; East ]
    | _ -> []


let loop start =
    let rec follow parts from dir =
        let next = move from dir
        let nextDir = getChar next |> pipe |> List.except [ connection dir ]
        let parts = next :: parts

        match nextDir with
        | _ when next = start -> Some parts
        | [ nextDir ] -> follow parts next nextDir
        | _ -> None

    let loops =
        [ for dir in [ North; South; West; East ] do
              match follow [] start dir with
              | Some parts -> dir, parts
              | _ -> () ]

    let length = loops |> List.head |> snd |> List.length
    let startFragment = loops |> List.map fst

    let elements =
        [ for pos in loops |> List.head |> snd do
              pos, pos |> getChar |> pipe |> Set
          start, startFragment |> Set ]
        |> Map

    length, elements

let partOne =
    let length, _ = loop start
    length / 2

let partTwo =
    let _, loopElements = loop start

    let folder (count, n, s) pos =
        match loopElements |> Map.tryFind pos with
        | Some e ->
            let n = if e.Contains North then not n else n
            let s = if e.Contains South then not s else s
            count, n, s
        | _ ->
            let count = if n || s then count + 1 else count
            count, n, s

    [ for y in 0 .. H - 1 do
          let countRow, _, _ =
              [ for x in 0 .. W - 1 -> x, y ] |> List.fold folder (0, false, false)

          countRow ]
    |> List.sum
