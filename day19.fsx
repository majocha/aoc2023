#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "19.txt"

type Part = { X: int; M: int; A: int; S: int }

type Cat = X | M | A | S

let getC (p: Part) = function X -> p.X | M -> p.M | A -> p.A| S -> p.S

type Condition =
    | Gt of int
    | Lt of int

type Rule = { Condition: Condition; Category: Cat;  Next: string }

type Workflow = Workflow of Rule list * next: string

let (|GT|_|) = trySscanf "%s>%d:%s"
let (|LT|_|) = trySscanf "%s<%d:%s"

let (|Cat|) = function "x" -> X | "m" -> M | "a" -> A | "s" -> S | _ -> failwith "wrong category"

let rec parseWorkflow wfs =
    let rec parseRules acc =
        function
        | GT(Cat c, d, next) :: rest -> parseRules ({Condition = Gt d; Category = c; Next = next } :: acc) rest
        | LT(Cat c, d, next) :: rest -> parseRules ({Condition = Lt d; Category = c; Next = next } :: acc) rest
        | [ next ] -> Workflow(acc |> List.rev, next)
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
    { X = x; M = m; A = a; S = s }

let workflows, parts =
    let workflows, input = input |> List.ofSeq |> parseWorkflows
    Map workflows, input |> List.map parsePart

module PartOne = 
    let (|NextWf|_|) p (rule: Rule) =
        match rule.Condition, rule.Next with 
        | Gt d, w when (getC p rule.Category) > d -> Some w
        | Lt d, w when (getC p rule.Category) < d -> Some w
        | _ -> None

    let rec processRules part =
        function
        | NextWf part w :: _ -> processWorkflow part w
        | _ :: rules -> processRules part rules
        | [] -> None

    and processWorkflow part =
        function
        | "A" -> Some true
        | "R" -> Some false
        | w ->
            let (Workflow( rules, next)) = workflows[w]
            match processRules part rules with
            | None -> processWorkflow part next
            | x -> x

    let isAccepted part = processWorkflow part "in" |> Option.get

    let partOne = parts |> List.filter isAccepted |> List.sumBy (fun p -> p.X + p.M + p.A + p.S)


let filterByCondition = function Gt d -> (<) d | Lt d -> (>) d

let combinations m = m |> Map.map (fun _ s -> s |> Set.count |> int64) |> Map.values |> Seq.reduce ((*))

let rec acceptedFromWorkflow from w allowed =
    if allowed |> Map.values |> Seq.exists Set.isEmpty then 0L
    else
    match w with
    | "A" -> combinations allowed
    | "R" -> 0L
    | w ->
        let (Workflow( rules, next)) = workflows[w]
        let results, last =
            rules |> List.mapFold(fun p r ->
                let filtered = 
                    p |> Map.map (fun k v -> if k = r.Category then v |> Set.filter (filterByCondition r.Condition) else v)
                let rest = p |> Map.map (fun k v -> if k = r.Category then v |> Set.filter (filterByCondition r.Condition >> not) else v)
                filtered |> acceptedFromWorkflow w r.Next, rest)
                allowed
        let results = (acceptedFromWorkflow w next last) :: results
        results |> sum

let countCombinations = acceptedFromWorkflow "" "in"

let possible = [X; M; A; S] |> List.map( fun c -> c, Set [1 .. 4000]) |> Map

let partTwo =countCombinations possible
