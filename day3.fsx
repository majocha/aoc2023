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

numbersWithSymbols |> sum

