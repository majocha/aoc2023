#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "25.txt"

let V, E =
    input
    |> Seq.fold
        (fun (vertices, edges) line ->
            let xs = line.Replace(": ", " ").Split(' ')
            vertices + Set xs, edges + Set [ for w in xs[1..] -> xs[0], w ])
        (Set.empty, Set.empty)

let rnd = System.Random()

let rec runPartition n =
    let ss subsets x = subsets |> Seq.find (Set.contains x)
    let randomEdge () = E |> Seq.item (rnd.Next E.Count)

    let rec partition subsets =
        if subsets |> Set.count <= 2 then subsets else
            let u, v = randomEdge ()
            let s1, s2 = ss subsets u, ss subsets v 
            if s1 <> s2 then
                subsets |> Set.remove s1 |> Set.remove s2 |> Set.add (s1 + s2)
            else subsets
            |> partition

    let subsets = V |> Set.map Set.singleton
    let parts = partition subsets
    if E |> Set.filter (fun (u, v) -> ss parts u <> ss parts v) |> Set.count < 4 then
        parts |> Set.map Set.count
    else
        parts |> Set.map Set.count |> printfn "retrying %d, %A" n
        runPartition (n + 1)

let partOne = runPartition 1 |> Seq.reduce ((*))
