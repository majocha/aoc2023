#time "on"
let input = System.IO.File.ReadAllLines("13.txt")

let pats =
    let folder (acc, lines) line =
        match line |> Seq.toList with
        | [] -> ((lines |> List.rev) :: acc, [])
        | line -> acc, line :: lines

    let result, last = input |> Seq.fold folder ([], [])
    ((last |> List.rev) :: result) |> List.rev

let diffByOneChar a b =
    let rec check d a b =
        match a, b with
        | [], [] -> d
        | c1 :: a, c2 :: b when c1 <> c2 -> if d then false else check true a b
        | _ :: a, _ :: b -> check d a b
        | _ -> false

    check false a b

let isReflection part pat n =
    let rec check fixUsed =
        function
        | [], _
        | _, [] -> fixUsed
        | h1 :: m1, h2 :: m2 when h1 = h2 -> check fixUsed (m1, m2)
        | h1 :: m1, h2 :: m2 when not fixUsed && diffByOneChar h1 h2 -> check true (m1, m2)
        | _ -> false

    check (part = 1) (pat |> List.take n |> List.rev, pat |> List.skip n)

let findReflection part pat =
    match [ 1 .. (pat |> List.length) - 1 ] |> List.tryFind (isReflection part pat) with
    | Some n -> n
    | _ -> 0

let result part =
    [ for p in pats -> 100 * (findReflection part p) + (p |> List.transpose |> findReflection part) ]
    |> List.sum

let partOne = result 1
let partTwo = result 2
