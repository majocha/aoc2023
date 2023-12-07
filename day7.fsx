#time
#r "nuget: FSharpPlus"
open FSharpPlus

let input = System.IO.File.ReadAllLines("7.txt")

let handType s =
    let hand s = s |> Seq.countBy id |> toList |> sortBy snd |> rev
    match hand s |> map snd |> toList with
    | [ 5 ] -> 7
    | [ 4; 1] -> 6
    | [ 3; 2 ] -> 5
    | 3 :: _ -> 4
    | [ 2; 2; 1] -> 3
    | 2 :: _ -> 2
    | _ -> 1

let secondOrder (s: string) =
    let cardStrength c = "AKQJT98765432" |> rev |> Seq.findIndex ((=) c)
    s |> Seq.map cardStrength |> toList

let strength s = handType s :: secondOrder s

//let strength s =
//    let cardStrength c = "AKQJT98765432" |> rev |> Seq.findIndex ((=) c)
//    let hand s = s |> Seq.countBy id |> toList |> sortBy snd |> rev
//    [ for c, n in hand s do
//        cardStrength c * pown 13 (n - 1) ]
//    |> sum

let partOne = 
    [ for s, d in input |> map (sscanf "%s %d") -> strength s, d ]
    |> sortBy fst |> map snd |> List.indexed
    |> List.sumBy (fun (i, s) -> (i + 1) * s)


