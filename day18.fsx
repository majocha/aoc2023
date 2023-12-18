#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "18.txt"

type Move =
    | D of int
    | U of int
    | L of int
    | R of int

let parseMove c steps =
    match c with
    | 'D' -> D steps
    | 'U' -> U steps
    | 'L' -> L steps
    | 'R' -> R steps
    | _ -> failwith "wrong char"

type Mark =
    | Include of int * int
    | Exclude of int * int

let (|ConvexStart|_|) =
    function
    | R r :: rest -> Some(true, R r :: rest)
    | L l :: rest -> Some(false, L l :: rest)
    | _ -> None

let (|ConvexEnd|_|) =
    function
    | R r :: rest -> Some(false, R r :: rest)
    | L l :: rest -> Some(true, L l :: rest)
    | _ -> None

let (|Horizontal|_|) =
    function
    | R r :: rest -> Some(r, rest)
    | L l :: rest -> Some(-l, rest)
    | _ -> None

let (|Exclusion|_|) =
    function
    | ConvexStart(q1, Horizontal(dx, D d :: ConvexEnd(q2, rest))) -> Some(dx, d, q1, q2, rest)
    | _ -> None

let (|Inclusion|_|) =
    function
    | Horizontal(dx, U u :: rest) -> Some(dx, -u, rest)
    | _ -> None

let rec dig result (x, y) plan =
    match plan with
    | [] -> result
    | Inclusion(dx, dy, rest) -> dig ((x + dx, Include(y + dy, y)) :: result) (x + dx, y + dy) rest
    | Exclusion(dx, dy, q1, q2, rest) ->
        dig
            ((x + 1 + dx, Exclude(y + (if q1 then 0 else 1), y + dy - (if q2 then 0 else 1)))
             :: result)
            (x + dx, y + dy)
            rest
    | rest -> failwithf $"wrong plan {rest}"

let calculateVol plan =
    let result = dig [] (0, 0) plan

    let changes =
        result
        |> List.groupBy fst
        |> List.sortBy fst
        |> List.map (fun (x, kvs) -> x, kvs |> List.map snd)

    let calculate (prevX, vol, column) (x, changes) =
        let xd = x - prevX |> int64
        let vol = vol + xd * (column |> Set.count |> int64)

        let column =
            changes
            |> List.fold
                (fun col ch ->
                    match ch with
                    | Include(y1, y2) -> col + Set [ y1..y2 ]
                    | Exclude(y1, y2) -> col - Set [ y1..y2 ])
                column

        (x, vol, column)

    let _, vol, _ = changes |> List.fold calculate (0, 0L, Set.empty)
    vol

let partOne =
    let plan =
        input
        |> Seq.map (sscanf "%c %d %s")
        |> Seq.map (fun (c, d, _) -> parseMove c d)
        |> Seq.toList

    calculateVol plan

let partTwo =
    let decode (str: string) =
        parseMove
            (match str[6] with
             | '0' -> 'R'
             | '1' -> 'D'
             | '2' -> 'L'
             | '3' -> 'U'
             | _ -> failwithf $"{str} wrong char")
            (str[1..5] |> sscanf "%x")

    let plan =
        input
        |> Seq.map (sscanf "%c %d (%s)")
        |> Seq.map (fun (_, _, h) -> decode h)
        |> Seq.toList

    calculateVol plan
