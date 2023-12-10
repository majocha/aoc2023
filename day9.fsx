#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines("9.txt")

let history line =
    line |> String.split [ " " ] |> Seq.map int |> toList

let diff step =
    [ for a, b in step |> List.pairwise -> b - a ]

let predict line =
    let rec predict' lastVs line =
        let lastV = line |> Seq.last
        let lastVs = (lastV :: lastVs)

        if line |> List.forall ((=) 0) then
            lastVs |> List.reduce (+)
        else
            let line = diff line
            predict' lastVs line

    predict' [] line

let partOne = input |> map (history >> predict) |> sum

let partTwo = input |> map (history >> rev >> predict) |> sum
