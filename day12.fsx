#time "on"
#nowarn "40"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines("12.txt")

let repeatString sep n s =
    [ for i in 1..n -> s ] |> String.concat sep

let parseLine n line =
    let pattern, groups = sscanf "%s %s" line
    let pattern, groups = repeatString "?" n pattern, repeatString "," n groups
    pattern, groups.Split(',') |> Seq.map int |> Seq.toList

let matchChar c =
    function
    | c1 :: rest when c1 = '?' || c1 = c -> Some rest
    | _ -> None

let rec matchN n f input =
    match f input with
    | _ when n = 0 -> Some input
    | Some rest when n = 1 -> Some rest
    | Some rest -> matchN (n - 1) f rest
    | None -> None

let (|Hash|_|) n = matchN n (matchChar '#')
let (|Dot|_|) n = matchN n (matchChar '.')

let (|Group|_|) padLeft padRight hashes =
    function
    | Dot padLeft (Hash hashes (Dot padRight rest)) -> Some rest
    | _ -> None

let rec countMatches: int list -> char list -> int64 =
    fun gs pat ->
        let patLen = pat |> Seq.length
        let dots = patLen - (gs |> List.sum)

        match gs with
        | [] ->
            printfn "%s" (pat |> String.ofList)
            1L

        | [ g ] ->
            [ for i in 0..dots do
                  let padRight = (dots - i)

                  match pat with
                  | Group i padRight g [] -> 1L
                  | _ -> 0L ]
            |> List.sum


        | g :: grest ->
            [ for i in 0..dots do
                  match pat with
                  | Group i 1 g rest -> countMatches grest rest
                  | _ -> 0L ]
            |> List.sum
    |> memoizeN

let countAll rep =
    [ for line in input do
          let pat, gs = parseLine rep line
          countMatches gs (pat |> Seq.toList) ]
    |> List.sum

let partOne = countAll 1
let partTwo = countAll 5
