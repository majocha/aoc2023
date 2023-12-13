#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines("13.txt")

let pats =
    let folder (acc, lines) line =
        if line = "" then
            ((lines |> List.rev) :: acc, [])
        else
            (acc, line :: lines)

    let result, last = input |> Seq.fold folder ([], [])
    ((last |> List.rev) :: result) |> List.rev


let getColumn n (pat: string list) =
    [ for i in 0 .. (pat |> List.length) - 1 -> pat[i][n] ] |> String.ofSeq

let rotate pat =
    [ for i in 0 .. (pat |> List.head |> String.length) - 1 -> getColumn i pat ]

let diffByOneChar a b =
    let rec check d a b =
        match a, b with
        | [], [] -> d
        | c1 :: a, c2 :: b when c1 <> c2 -> if d then false else check true a b
        | _ :: a, _ :: b -> check d a b
        | _ -> false

    check false (a |> List.ofSeq) (b |> List.ofSeq)

let isReflection (pat: string list) n =
    let rec check fixUsed =
        function
        | [], _
        | _, [] -> fixUsed
        | h1 :: m1, h2 :: m2 when h1 = h2 -> check fixUsed (m1, m2)
        | h1 :: m1, h2 :: m2 when not fixUsed && diffByOneChar h1 h2 -> check true (m1, m2)
        | _ -> false

    check false (pat[0..n] |> List.rev, pat[n + 1 ..])

let findReflection pat =
    match [ 0 .. (pat |> List.length) - 1 ] |> List.tryFind (isReflection pat) with
    | Some n -> n + 1
    | _ -> 0

let partTwo =
    [ for p in pats -> 100 * (findReflection p) + (p |> rotate |> findReflection) ]
    |> List.sum
