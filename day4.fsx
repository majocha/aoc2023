#time
#r "nuget: FSharpPlus"

open FSharpPlus
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "4.txt"

let cards =
    [ for line in input do
          let i, s1, s2 = sscanf "Card %d: %s| %s" line
          let winning = Regex.Matches(s1, "(\d+)") |> Seq.map _.Value |> map parse<int> |> Set
          let your = Regex.Matches(s2, "(\d+)") |> Seq.map _.Value |> map parse<int> |> Set
          i, winning, your ]

let matches =
    [ for i, winning, your in cards do
          Set.intersect winning your |> length ]

let points n = pown 2 (n - 1)

let partOne = matches |> map points |> sum

let totalCards = Array.create cards.Length 1

for i, score in matches |> List.indexed do
    for j in i + 1 .. i + score do
        if j < cards.Length then
            totalCards[j] <- totalCards[j] + totalCards[i]

let partTwo = sum totalCards
