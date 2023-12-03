#time
#r "nuget: FSharpPlus"

open FSharpPlus

open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "3.txt" |> String.concat ""

let D = 10


let numbers input =
    [ for m in Regex.Matches(input, @"\d+") -> m.Value, m.Index]

let symbols input =
    [ for m in Regex.Matches(input, @"[^\d|\.]") -> m.Value, m.Index]


numbers input

symbols input