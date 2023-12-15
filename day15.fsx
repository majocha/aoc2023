#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input =
    // "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    System.IO.File.ReadAllText "15.txt"

let init = input.Split(',')

let hashChar current c = (current + int c) * 17 % 256

let hash str = str |> Seq.fold hashChar 0

let partOne = init |> Seq.map hash |> Seq.sum

let (|Remove|_|) = trySscanf "%s-"

let (|AddReplace|_|) = trySscanf "%s=%d"

type Box(kvs) =
    member x.Remove k =
        Box(kvs |> List.filter (fun kv -> fst kv <> k))

    member x.Add(k, v) =
        if kvs |> List.map fst |> List.contains k then
            [ for k1, v1 in kvs do
                  if k1 = k then k, v else k1, v1 ]
        else
            [ yield! kvs; k, v ]
        |> Box

    member x.Power =
        kvs |> List.indexed |> List.map (fun (i, (_, v)) -> (i + 1) * v) |> List.sum

    override _.ToString() = kvs.ToString()

let step boxes str =
    // printfn "\n%s" str
    match str with
    | Remove label ->
        let h = hash label
        let a = boxes |> Map.tryFind h |> Option.defaultValue (Box [])
        boxes.Add(h, a.Remove label)
    | AddReplace(label, fl) ->
        let h = hash label
        let a = boxes |> Map.tryFind h |> Option.defaultValue (Box [])
        boxes.Add(h, a.Add(label, fl))
    | _ -> failwith "invalid input"
// |> tap (Map.toSeq >> Seq.iter (printfn "%O"))

let result boxes =
    boxes |> Map.toSeq |> Seq.sumBy (fun (i, v: Box) -> v.Power * (i + 1))

let partTwo = init |> Seq.fold step Map.empty |> result
