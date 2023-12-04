#time
#r "nuget: FSharpPlus"
open FSharpPlus
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "4.txt"

let cards =
    [ for line in input do
        let i, s1,s2 = sscanf "Card %d: %s| %s" line
        let winning = Regex.Matches (s1, "(\d+)") |> Seq.map _.Value |> map parse<int> |> Set
        let your = Regex.Matches (s2, "(\d+)") |> Seq.map _.Value |> map parse<int> |> Set
        i, winning, your ]

let points =
    [ for i, winning, your in cards do
        let matches = Set.intersect winning your |> length
        pown 2 (matches - 1)
    ]

let partOne = points |> sum





