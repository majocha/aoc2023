#time
#r "nuget: FSharpPlus"

open FSharpPlus

module PartOne =
    let input = System.IO.File.ReadAllLines("7.txt")

    let handType s =
        let hand s =
            s |> Seq.countBy id |> toList |> sortBy snd |> rev

        match hand s |> map snd |> toList with
        | [ 5 ] -> 7
        | [ 4; 1 ] -> 6
        | [ 3; 2 ] -> 5
        | 3 :: _ -> 4
        | [ 2; 2; 1 ] -> 3
        | 2 :: _ -> 2
        | _ -> 1

    let secondOrder (s: string) =
        let cardStrength c =
            "AKQJT98765432" |> rev |> Seq.findIndex ((=) c)

        s |> Seq.map cardStrength |> toList

    let strength s = handType s :: secondOrder s

    let partOne =
        [ for s, d in input |> map (sscanf "%s %d") -> strength s, d ]
        |> sortBy fst
        |> map snd
        |> List.indexed
        |> List.sumBy (fun (i, s) -> (i + 1) * s)


let input = System.IO.File.ReadAllLines("7.txt")

let handType s =
    let jokers = s |> Seq.filter ((=) 'J') |> length
    let s = s |> Seq.filter ((<>) 'J')
    let hand = s |> Seq.countBy id |> toList |> sortBy snd |> rev
    let hand = hand |> map snd |> toList

    let hand =
        match hand with
        | [] -> [ 5 ]
        | x :: rest -> (x + jokers) :: rest

    match hand with
    | [ 5 ] -> 7
    | [ 4; 1 ] -> 6
    | [ 3; 2 ] -> 5
    | 3 :: _ -> 4
    | [ 2; 2; 1 ] -> 3
    | 2 :: _ -> 2
    | _ -> 1

let secondOrder (s: string) =
    let cardStrength c =
        "AKQT98765432J" |> rev |> Seq.findIndex ((=) c)

    s |> Seq.map cardStrength |> toList

let strength s = handType s :: secondOrder s

let partTwo =
    [ for s, d in input |> map (sscanf "%s %d") -> strength s, d ]
    |> sortBy fst
    |> map snd
    |> List.indexed
    |> List.sumBy (fun (i, s) -> (i + 1) * s)
