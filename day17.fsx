#time "on"

let grid =
    System.IO.File.ReadAllLines "17.txt"
    |> Seq.toList
    |> List.map (Seq.map (fun c -> int c - int '0') >> Seq.toList)

let gridSize = grid.Length

type Dir =
    | Up
    | Down
    | Left
    | Right

type TurnDir =
    | TurnLeft
    | TurnRight

type Pos = int * int

type Node =
    { Position: Pos
      Direction: Dir
      Steps: int }

let getWeight (x, y) = grid[x][y]

let turn dir turnDir =
    let dirs = [ Up; Right; Down; Left ]
    let i = dirs |> List.findIndex ((=) dir)

    ((match turnDir with
      | TurnLeft -> 4 + i - 1
      | TurnRight -> i + 1) % 4,
     dirs)
    ||> List.item

let step from dir =
    let x, y = from

    let x, y =
        match dir with
        | Up -> x, y - 1
        | Down -> x, y + 1
        | Left -> x - 1, y
        | Right -> x + 1, y

    let inBounds d = d >= 0 && d < gridSize
    if inBounds x && inBounds y then Some(x, y) else None

let nextNodes maxSteps stepsToTurn (node: Node) =
    let nextDirs =
        [ if node.Steps >= stepsToTurn then
              turn node.Direction TurnLeft
          if node.Steps >= stepsToTurn then
              turn node.Direction TurnRight

          if node.Steps < maxSteps then
              node.Direction ]

    [ for d in nextDirs do
          match step node.Position d with
          | Some pos ->
              { Position = pos
                Direction = d
                Steps = if d <> node.Direction then 1 else node.Steps + 1 }
          | _ -> () ]

let rec path nextNodes visited boundary =
    let updates =
        [ for node in boundary do
              let loss = visited |> Map.tryFind node |> Option.defaultValue 0

              for next in nextNodes node do
                  let w = getWeight next.Position
                  let nextLoss = loss + w

                  match visited.TryFind next with
                  | None -> next, nextLoss
                  | Some l when nextLoss < l -> next, nextLoss
                  | _ -> () ]
        |> List.groupBy fst
        |> List.map (snd >> List.minBy snd)

    let visited =
        updates |> List.fold (fun visited (k, v) -> visited |> Map.add k v) visited

    let boundary = updates |> List.map fst

    if boundary = [] then
        visited
    else
        path nextNodes visited boundary

let initial =
    [ { Position = (0, 0)
        Direction = Down
        Steps = 1 }
      { Position = (0, 0)
        Direction = Right
        Steps = 1 } ]

let partOne =
    path (nextNodes 3 0) Map.empty initial
    |> Map.filter (fun k _ -> k.Position = (gridSize - 1, gridSize - 1))
    |> Map.values
    |> Seq.min

let partTwo =
    path (nextNodes 10 4) Map.empty initial
    |> Map.filter (fun k _ -> k.Position = (gridSize - 1, gridSize - 1) && k.Steps >= 4)
    |> Map.values
    |> Seq.min
