#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "25.txt"

let V, E' = input |> Seq.fold(fun (vs, es) line ->
    let (v :: ws) = line |> String.replace ": " " " |> String.split [" "] |> List.ofSeq
    vs + Set (v :: ws) , es + Set [for w in ws -> v, w] ) (Set.empty, Set.empty)

let E = E' |> List.ofSeq
let rnd = System.Random()

let rec runPartition () =
    let ss subsets x = subsets |> Seq.find (Set.contains x)
    let rec partition subsets =
        if subsets |> Set.count <= 2 then subsets else
            let u, v = E[rnd.Next E.Length]
            let s1, s2 = ss subsets v, ss subsets u
            if s1 <> s2 then
                subsets |> Set.remove s1 |> Set.remove s2 |> Set.add (s1 + s2)
            else 
                subsets
            |> partition

    let parts = V |> Set.map Set.singleton |> partition
    if E |> List.filter (fun (u, v) -> ss parts u <> ss parts v) |> List.length < 4 then parts
    else
        runPartition()

let partOne = runPartition() |> Set.map Set.count |> Seq.reduce ((*))








