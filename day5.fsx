#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "5.txt" |> toList

let intList s = s |> String.split [ " " ] |> Seq.map int64 |> toList

let seeds = input[0] |> sscanf "seeds: %s" |> intList

type Range = { Dest: int64; Source: int64; Length: int64 }
    with
    member r.Contains x = x >= r.Source && x < r.Source + r.Length
    member r.Convert x = r.Dest + x - r.Source


let maps =
    [ for groups in input |> skip 2 |> split [ [ "" ] ]  do
        [ for line in groups |> skip 1 do
            let d,s, l = sscanf "%d %d %d" line 
            { Dest  =d; Source = s; Length = l }
        ] ]

let convertSeed (ranges: Range list) x =
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



convert [79] maps









