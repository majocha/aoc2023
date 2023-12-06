#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "5.txt" |> toList

let intList s =
    s |> String.split [ " " ] |> Seq.map int64 |> toList

module PartOne = 

    let seeds = input[0] |> sscanf "seeds: %s" |> intList

    type Mapping' =
        { Dest: int64
          Source: int64
          Length: int64 }

        member r.Contains x =
            x >= r.Source && x < r.Source + r.Length

        member r.Convert x = r.Dest + x - r.Source

    let maps =
        [ for groups in input |> skip 2 |> split [ [ "" ] ] do
              [ for line in groups |> skip 1 do
                    let d, s, l = sscanf "%d %d %d" line
                    { Dest = d; Source = s; Length = l } ] ]

    let convertSeed (ranges: Mapping' list) x =
        match ranges |> List.tryFind (fun r -> r.Contains x) with
        | Some r -> r.Convert x
        | _ -> x

    let convertOnce seeds ranges = seeds |> List.map (convertSeed ranges)

    let rec convert seeds =
        function
        | [] -> seeds
        | ranges :: maps ->
            let seeds = convertOnce seeds ranges
            convert seeds maps

    let partOne = convert seeds maps |> List.min


type Range = Range of start: int64 * stop: int64

type Mapping = Mapping of Range * offset: int64

type Marker =
    | Start of int64
    | End of int64
    | StartOffset of int64 * int64
    | EndOffset of int64

    member m.Position =
        match m with
        | Start p
        | End p
        | StartOffset(p, _)
        | EndOffset p -> p

    member m.Rank =
        match m with
        | Start _ -> 2
        | End _ -> -2
        | StartOffset _ -> 1
        | EndOffset _ -> -1

    member m.Order =
        match m with
        | Start _ -> 0
        | End _ -> 2
        | StartOffset _ -> 1
        | EndOffset _ -> 3

    member m.Offset =
        match m with
        | StartOffset(_, o) -> o
        | _ -> 0L

let rec parseSeedRanges result =
    function
    | s :: l :: rest -> parseSeedRanges (Range(s, s + l - 1L) :: result) rest
    | [] -> result |> rev
    | _ -> failwith "wrong input"

let initialRanges = parseSeedRanges [] (input[0] |> sscanf "seeds: %s" |> intList)

let mappings: Mapping list list =
    [ for groups in input |> skip 2 |> split [ [ "" ] ] do
          [ for line in groups |> skip 1 do
                let d, s, l = sscanf "%d %d %d" line
                Mapping(Range(s, s + l - 1L), d - s) ] ]

let convert ranges mappings =
    let markers =
        [ for Range(s, e) in ranges do
              Start s
              End e
          for Mapping(Range(s, e), offset) in mappings do
              StartOffset(s, offset)
              EndOffset e ]
        |> sortBy _.Order
        |> sortBy _.Position

    printfn "%A" markers

    let rec cut v start result markers =

        let s, v1 =
            match markers with
            | (m: Marker) :: _ ->
                let s = m.Position
                let rank = m.Rank
                s, v + rank
            | [] -> 0L, v

        let changed = v1 <> v

        let result =
            match start with
            | Some(s1) when changed -> Range(s1, s) :: result
            | _ -> result

        let start = if changed && v1 >= 2 then Some s else None

        match markers with
        | _ :: markers -> cut v1 start result markers
        | [] -> result |> rev

    let cutRanges = cut 0 None [] markers

    [ for Range(s, e) in cutRanges do
          match
              mappings
              |> List.tryFind (fun (Mapping(Range(s1, e1), offset)) -> s >= s1 && e <= e1)
          with
          | Some(Mapping(_, offset)) -> Range(s + offset, e + offset)
          | _ -> Range(s, e) ]

let rec convertAll ranges =
    function
    | [] -> ranges
    | mapping :: mappings ->
        let ranges = convert ranges mapping
        convertAll ranges mappings

let partTwo = convertAll initialRanges mappings |> List.min
