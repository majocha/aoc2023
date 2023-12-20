#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "19.txt"

type Cat =
    | X
    | M
    | A
    | S

type Part = Map<Cat, int>

let getRating (p: Part) cat = p[cat]

type Rule =
    | Gt of Cat * int * string
    | Lt of Cat * int * string
    | Next of string

let (|GT|_|) = trySscanf "%s>%d:%s"
let (|LT|_|) = trySscanf "%s<%d:%s"

let (|Cat|) =
    function
    | "x" -> X
    | "m" -> M
    | "a" -> A
    | "s" -> S
    | _ -> failwith "wrong category"

let rec parseWorkflow wfs =
    let rec parseRules acc =
        function
        | GT(Cat c, d, next) :: rest -> parseRules (Gt(c, d, next) :: acc) rest
        | LT(Cat c, d, next) :: rest -> parseRules (Lt(c, d, next) :: acc) rest
        | [ next ] -> (Next next) :: acc |> List.rev
        | _ -> failwith "invalid rule list"

    function
    | "" :: rest -> wfs, rest
    | line :: rest ->
        let name, str = sscanf "%s{%s}" line
        let wf = str.Split(',') |> List.ofSeq |> (parseRules [])
        parseWorkflow ((name, wf) :: wfs) rest
    | _ -> failwith "invalid input"

let parseWorkflows = parseWorkflow []

let parsePart str =
    let x, m, a, s = sscanf "{x=%d,m=%d,a=%d,s=%d}" str
    Part [ X, x; M, m; A, a; S, s ]

let workflows, parts =
    let workflows, rest = input |> List.ofSeq |> parseWorkflows
    Map workflows, rest |> List.map parsePart

module PartOne =

    let rec processRules part =
        function
        | Gt(c, d, w) :: _ when (getRating part c) > d -> processWorkflow part w
        | Lt(c, d, w) :: _ when (getRating part c) < d -> processWorkflow part w
        | [ Next w ] -> processWorkflow part w
        | _ :: rules -> processRules part rules
        | [] -> failwith "invalid rule list"

    and processWorkflow part =
        function
        | "A" -> true
        | "R" -> false
        | w -> processRules part workflows[w]

    let isAccepted part = processWorkflow part "in"

    let partOne =
        parts |> List.filter isAccepted |> List.sumBy (fun p -> p.Values |> Seq.sum)


let applyRule k =
    function
    | Gt(c, d, _) when c = k -> (<) d
    | Lt(c, d, _) when c = k -> (>) d
    | _ -> fun _ -> true

let combinations m =
    m
    |> Map.map (fun _ s -> s |> Set.count |> int64)
    |> Map.values
    |> Seq.reduce ((*))

let rec acceptedFromWorkflow ratings =
    function
    | "A" -> combinations ratings
    | "R" -> 0L
    | w ->
        workflows[w]
        |> List.fold
            (fun (p, count) r ->
                match r with
                | Gt(c, d, w)
                | Lt(c, d, w) ->
                    let filtered = p |> Map.add c (p[c] |> Set.filter (applyRule c r))
                    let rest = p |> Map.add c (p[c] |> Set.filter (applyRule c r >> not))
                    rest, count + acceptedFromWorkflow filtered w
                | Next w -> p, count + acceptedFromWorkflow p w)
            (ratings, 0L)
        |> snd

let countCombinations possible = acceptedFromWorkflow possible "in"

let possible = [ X; M; A; S ] |> List.map (fun c -> c, Set [ 1..4000 ]) |> Map

let partTwo = countCombinations possible
