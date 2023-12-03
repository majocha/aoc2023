#time
#r "nuget: FSharpPlus"

open FSharpPlus

open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "3.txt" |> String.concat ""

let D = 140

let numbers = Regex.Matches(input, @"\d+") |> Seq.toList
let symbols = Regex.Matches(input, @"[^\d|\.]") |> Seq.toList

let symbolIndexes = symbols |> map _.Index |> Set

let numbersWithSymbols: int list =
    [
        for m in numbers do
            let i, l = m.Index, m.Length
            let ext = [i - 1 .. i + l] @  [i - 1 - D .. i + l - D] @ [i - 1 + D .. i + l + D] |> Set
            if not (Set.intersect symbolIndexes ext |> Set.isEmpty) then parse m.Value
    ]

let partOne = numbersWithSymbols |> sum

let starSymbols = Regex.Matches(input, @"\*") |> Seq.map _.Index |> Set

let adjMap =
    [
        for m in numbers do
            let i, l = m.Index, m.Length
            let ext = [i - 1 .. i + l] @  [i - 1 - D .. i + l - D] @ [i - 1 + D .. i + l + D]
            for i in ext -> i, (parse m.Value : int)
    ]
    |> List.groupBy fst
    |> List.filter (fun (i, vs) -> vs.Length = 2)
    |> List.map (fun (i, vs) -> i, [ for _, v in vs -> v])
    |> Map

let matchingStars = Set.intersect (Set adjMap.Keys) starSymbols

let partTwo =
    [
        for i in matchingStars do
            let vs = adjMap[i]
            vs[0] * vs[1]
    ]
    |> sum

