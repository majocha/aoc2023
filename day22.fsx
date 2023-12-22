#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "22.txt"

let parseBrick str =
    let x1, y1, z1, x2, y2, z2 = sscanf "%d,%d,%d~%d,%d,%d" str

    [ for x in x1..x2 do
          for y in y1..y2 do
              for z in z1..z2 do
                  x, y, z ]

let trd (a, b, c) = c

let bricks = input |> Seq.map parseBrick |> Seq.toList

let bottom brick = brick |> List.map trd |> List.min

let top brick = brick |> List.map trd |> List.max

bricks |> List.sortBy top = bricks

let projxy brick =
    brick |> List.map (fun (x, y, z) -> (x, y)) |> Set

let overlaping b1 b2 =
    projxy b1 |> Set.intersect (projxy b2) |> Set.isEmpty |> not

let findSupporting brick stack =
    stack |> List.sortByDescending top |> List.tryFind (fun b -> overlaping b brick)

let drop stack brick =
    let hmin = findSupporting brick stack |> Option.map top |> Option.defaultValue 0
    let a = bottom brick
    let dropped = brick |> List.map (fun (x, y, z) -> x, y, z - a + hmin + 1)
    dropped :: stack

let dropStack bricks =
    bricks |> List.sortBy bottom |> List.fold drop []

let dropped = dropStack bricks

let isSupported b a = bottom a = top b + 1 && overlaping a b

let supportMap stack =
    let gb = List.groupBy fst >> List.map (fun (k, g) -> k, g |> List.map snd) >> Map

    let rec loop supports supported =
        function
        | [] -> (gb supports), (gb supported)
        | (bi, b) :: above ->
            let supportedBy = above |> List.filter (snd >> isSupported b) |> List.map fst

            let supports, supported =
                supportedBy
                |> List.fold (fun (ba, ab) i -> (bi, i) :: ba, (i, bi) :: ab) (supports, supported)

            loop supports supported above

    stack |> List.indexed |> List.rev |> loop [] []


let supports, supported = supportMap dropped
let canBeRemoved b =
    not (supports.ContainsKey b)
    || supports[b] |> List.forall (fun a -> supported[a] |> List.length > 1)

let indexes = [ 0 .. dropped.Length - 1 ]

let partOne = indexes |> List.filter canBeRemoved |> List.length

let rec wouldFall fell b =
    match supports |> Map.tryFind b with
    | None -> fell
    | Some above ->
        let immidiately = above |> List.filter (fun a -> Set supported[a] - (fell |> Set.add b) |> Set.isEmpty)
        immidiately |> List.fold wouldFall (Set immidiately + fell)

let partTwo = indexes |> List.filter (canBeRemoved >> not) |> List.sumBy (wouldFall Set.empty >> Set.count)
