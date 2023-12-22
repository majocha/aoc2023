#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "22.txt"
let parseBrick str = 
    let x1,y1,z1,x2,y2,z2 = sscanf "%d,%d,%d~%d,%d,%d" str
    [
        for x in x1 .. x2 do
            for y in y1 .. y2 do
                for z in z1 .. z2 do
                    x, y, z
    ]

let trd (a,b,c) = c 

let bricks = input |> Seq.map parseBrick |> Seq.toList

let alt brick = brick |> List.map trd |> List.min

let height brick = brick |> List.map trd |> List.max

let projxy brick = brick |> List.map (fun (x,y,z) -> (x,y)) |> Set

let overlaping b1 b2 = Set.isEmpty (projxy b1 - projxy b2) |> not

let findSupporting brick stack = stack |> List.tryFind (fun b -> overlaping b brick)

let drop stack brick =
    let hmin = findSupporting brick stack |> Option.map height |> Option.defaultValue 0
    let a = alt brick
    let dropped = brick |> List.map (fun (x, y, z) -> x, y, z - a + hmin + 1)
    dropped :: stack

let dropStack bricks = bricks |> List.sortBy alt |> List.fold drop []

let dropped = dropStack bricks

let supported (b1, b2) = alt b1 = height b2 + 1 && overlaping b1 b2

let allSupported stack = stack |> List.pairwise |> List.forall supported

let canRemove stack b = stack |> List.except [b] |> allSupported

dropped |> List.filter (canRemove dropped) |> List.length



    



//let canDisintegrate dropped brick =
//    let without = dropped |> List.except [ brick ]
//    without = dropStack without

//dropped |> List.filter (canDisintegrate dropped) |> List.length