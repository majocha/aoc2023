#time "on"

let grid =
    System.IO.File.ReadAllLines "17.txt"
    |> Seq.toList
    |> List.map (Seq.map (fun c -> int c - int '0') >> Seq.toList)

let gridSize = grid.Length

let finalPos = gridSize - 1, gridSize - 1

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

let getLoss (x, y) = grid[x][y]

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

let rec path nextNodes endCondition visited boundary best =
    let nextBest =
        boundary
        |> List.fold (fun b (n, s) -> if endCondition n then min s b else b) best

    let nextBoundary =
        [ for from, currentLoss in boundary do
              for node in nextNodes from do
                  let oldLoss = visited |> Map.tryFind node |> Option.defaultValue nextBest
                  let nextLoss = currentLoss + getLoss node.Position

                  if oldLoss > nextLoss then
                      node, nextLoss ]
        |> List.groupBy fst
        |> List.map (snd >> List.minBy snd)

    let nextVisited =
        nextBoundary |> List.fold (fun visited (k, v) -> visited |> Map.add k v) visited

    if nextBoundary.IsEmpty then
        nextBest
    else
        path nextNodes endCondition nextVisited nextBoundary nextBest

let initial =
    [ { Position = (0, 0)
        Direction = Down
        Steps = 1 },
      0
      { Position = (0, 0)
        Direction = Right
        Steps = 1 },
      0 ]

let partOne =
    path (nextNodes 3 0) (fun n -> n.Position = finalPos) Map.empty initial 9999

let partTwo =
    path (nextNodes 10 4) (fun n -> n.Position = finalPos && n.Steps >= 4) Map.empty initial 9999
