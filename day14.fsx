#time "on"

#r "nuget: FSharpPlus"

open FSharpPlus

let toChunks row =
    let rec chunk acc =
        function
        | [] -> acc |> List.rev, []
        | '#' :: rest -> ('#' :: acc) |> List.rev, rest
        | c :: rest -> chunk (c :: acc) rest

    let (|Chunk|_|) = chunk [] >> Some

    let rec chunks acc =
        function
        | [] -> acc |> List.rev
        | Chunk(ch, rest) -> chunks (ch :: acc) rest
        | rest -> rest :: acc

    chunks [] row

let slideRow row =
    row |> toChunks |> List.map (List.sortByDescending id) |> List.concat

let north platform =
    platform |> List.transpose |> List.map slideRow |> List.transpose

let west platform = platform |> List.map slideRow

let south platform =
    platform
    |> List.transpose
    |> List.map (List.rev >> slideRow >> List.rev)
    |> List.transpose

let east platform =
    platform |> List.map (List.rev >> slideRow >> List.rev)

let initial =
    System.IO.File.ReadAllLines("14.txt") |> List.ofSeq |> List.map List.ofSeq

let totalLoad platform =
    let sumRow (i, row) =
        let stones = row |> List.filter ((=) 'O') |> List.length
        stones * (i + 1)

    platform |> List.rev |> List.indexed |> List.map sumRow |> List.sum

let cycle =
    fun p _ -> p |> north |> west |> south |> east
    |> memoizeN

let loads =
    [1 .. 1000] |> List.scan cycle initial |> List.map totalLoad

let isRepeating ls fragment =
    let repeated = [ for i in 1..10 do yield! fragment ]
    ls |> List.take repeated.Length = repeated

let findRepeat ls =
    let reversed = ls |> List.rev

    let cycleLength =
        [ 1..200 ] |> List.find (fun n -> reversed |> List.take n |> isRepeating reversed)

    let cycle = reversed |> List.take cycleLength |> List.rev

    let from =
        [ 0..200 ]
        |> List.find (fun n -> ls |> List.skip n |> List.take cycleLength = cycle)

    from, cycleLength

let partOne = initial |> north |> totalLoad

let partTwo =
    let from, cycleLength = findRepeat loads
    loads[(1000000000 - from) % cycleLength + from]

