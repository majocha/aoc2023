#time
#r "nuget: FSharpPlus"

open FSharpPlus

open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "3.txt" |> String.concat ""

let D = 140

let numbers = Regex.Matches(input, @"\d+") |> toList
let symbols = Regex.Matches(input, @"[^\d|\.]") |> toList

let symbolIndexes = symbols |> map _.Index |> Set

let neighbourhood index length =
    [index - 1 .. index + length] @
    [index - 1 - D .. index + length - D] @
    [index - 1 + D .. index + length + D]
    |> Set



let numbersWithSymbols: int list =
    [
        for m in numbers do
            if not ( neighbourhood m.Index m.Length |> Set.intersect symbolIndexes |> Set.isEmpty) then
                parse m.Value
    ]

let partOne = numbersWithSymbols |> sum

let starSymbols = Regex.Matches(input, @"\*") |> Seq.map _.Index |> Set

let adjMap =
    [
        for m in numbers do
            for i in neighbourhood m.Index m.Length ->
                i, (parse m.Value : int)
    ]
    |> List.groupBy fst
    |> List.filter (fun (i, vs) -> vs.Length = 2)
    |> List.map (fun (i, vs) -> i, (snd vs[0], snd vs[1]))
    |> Map

let matchingStars = Set.intersect (Set adjMap.Keys) starSymbols

let partTwo =
    [
        for i in matchingStars do
            let v1, v2 = adjMap[i]
            v1 * v2
    ]
    |> sum

