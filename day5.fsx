#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "5.txt" |> toList

let intList s = s |> String.split [ " " ] |> Seq.map int64 |> toList

let seeds = input[0] |> sscanf "seeds: %s" |> intList

type Range = { Dest: int64; Source: int64; Length: int64 }
    with
    member r.Contains x = x >= r.Source && x < r.Source + r.Length
    member r.TryConvert x =
        if r.Contains x then Some (r.Dest + x - r.Source) else None

let mapsInput =  input |> skip 3

let rec splitInput acc chunks = function
    | [] -> (acc |> rev) :: chunks |> rev
    | "" :: _ :: input -> splitInput [] ((acc |> rev) :: chunks) input
    | line :: input -> splitInput (line :: acc) chunks input

splitInput [] [] (input |> skip 3)

let maps =
    [ for lines in splitInput [] [] mapsInput  do
        [ for line in lines |> skip 1 do
            let d,s, l = sscanf "%d %d %d" line 
            { Dest  =d; Source = s; Length = l }
        ] ]

let convertSeed (ranges: Range list) x =
    let rec tryRange = function
    | (r: Range) :: ranges  ->
        match r.TryConvert x with
        | Some x1 -> x1
        | _ -> tryRange ranges
    | _ -> x
    tryRange ranges

let convertOnce seeds ranges = seeds |> List.map (convertSeed ranges)

let rec convert seeds = function
    | [] -> seeds
    | ranges :: maps ->
        let seeds = convertOnce seeds ranges
        printfn "%A" seeds
        convert seeds maps

let partOne = convert seeds maps













