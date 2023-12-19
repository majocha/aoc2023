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
        | [ next ] -> Workflow(acc, next)
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
 


//let applyCondition cat accepted =
//    function
//    | Gt(c, d) when c = cat -> accepted |> Set.filter ((>) d)
//    | Lt(c, d) when c = cat -> accepted |> Set.filter ((<) d)
//    | _ -> accepted


//let fitlerRules cat accepted rules =
//    rules |> List.fold (fun a ({ Condition = c; Next = next}) ->

//    ) accepted





//and rec filterParts c part =
//    function
//    | "A" -> part
//    | "R" -> Set.empty
//    | w -> filterRules c part workflows[w]
