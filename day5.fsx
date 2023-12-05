#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "5.txt" |> toList

let intList s = s |> String.split [ " " ] |> Seq.map int64 |> toList

let seeds = input[0] |> sscanf "seeds: %s" |> intList

type Mapping = { Dest: int64; Source: int64; Length: int64 }
    with
    member r.Contains x = x >= r.Source && x < r.Source + r.Length
    member r.Convert x = r.Dest + x - r.Source


let maps =
    [ for groups in input |> skip 2 |> split [ [ "" ] ]  do
        [ for line in groups |> skip 1 do
            let d,s, l = sscanf "%d %d %d" line 
            { Dest  =d; Source = s; Length = l }
        ] ]

let convertSeed (ranges: Mapping list) x =
    match ranges |> List.tryFind (fun r -> r.Contains x) with
    | Some r -> r.Convert x
    | _ -> x

let convertOnce seeds ranges = seeds |> List.map (convertSeed ranges)

let rec convert seeds = function
    | [] -> seeds
    | ranges :: maps ->
        printfn "%A" ranges
        let seeds = convertOnce seeds ranges
        printfn "%A" seeds
        convert seeds maps

let partOne = convert seeds maps |> List.min

type RangeMarker = Start | End
type Offset = Offset of int64

let rec parseSeedRanges (rangeMap: Map<int64, RangeMarker>) = function
    | s :: l :: rest ->
        let rangeMap = rangeMap |> Map.add s Start |> Map.add (s + l - 1L) End
        parseSeedRanges rangeMap rest
    | [] -> rangeMap
    | _ -> failwith "wrong input"

let initialSeedRanges = parseSeedRanges (Map []) (input[0] |> sscanf "seeds: %s" |> intList)

let mappings : Map<int64, Offset> list =
    [ for groups in input |> skip 2 |> split [ [ "" ] ]  do
        [ for line in groups |> skip 1 do
            let d, s, l = sscanf "%d %d %d" line
            s, Offset(d - s)
            s + l, Offset(0L)
        ] |> rev |> Map ]

let convertOnce' ranges mapping =
    let pois = [ ranges |> Map.keys; mapping |> Map.keys ] |> Seq.concat |> Set
    let mutable seed = false
    let mutable offset = 0L
    [ for p in pois do
        let ropt, oopt = (ranges |> Map.tryFind p), (mapping |> Map.tryFind p)
        match ropt, oopt with
        | None, Some (Offset o1) when seed ->
            p + offset, End
            offset <- o1
            p + offset, Start
        | None , Some (Offset o) -> offset <- o
        | Some Start, Some (Offset o) ->
            

    ] |> Map

let rec convert' ranges = function
    | [] -> ranges
    | mapping :: mappings ->
        let next = convertOnce' ranges mapping
        convert' next mappings

convert' initialSeedRanges mappings

convertOnce' initialSeedRanges mappings[0]