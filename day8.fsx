#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines("8.txt")

let instructions = input[0]

// prime number?
let lengthOfInstructions = instructions |> length |> int64

let nodes =
    [ for line in input |> skip 2 do
        let n, l, r = sscanf "%s = (%s, %s)" line
        n, (l, r) ]
    |> Map

let nextNode dir currentNode =
    match dir with
    | 'L' -> nodes[currentNode] |> fst
    | 'R' -> nodes[currentNode] |> snd
    | _ -> failwith "invalid instruction"

module PartOne = 
    let rec countSteps finish current step =
        if current = finish then step
        else
            let dir = instructions[ (int (step % lengthOfInstructions)) ]
            countSteps finish (nextNode dir current) (step + 1L)

    let partOne = countSteps "ZZZ" "AAA" 0



let starting = nodes.Keys |> filter (String.endsWith "A") |> toList

let ending = nodes.Keys |> filter (String.endsWith "Z") |> toList

let rec findNextZ current step =
    let dir = instructions[ (int (step % lengthOfInstructions)) ]
    let step = step + 1L
    let node = nextNode dir current
    if node |> String.endsWith "Z" then node, step
    else
        findNextZ node step

let findNextZ' = memoizeN findNextZ

let findNextZ'' current step =
    let z, increment = findNextZ' current (step % lengthOfInstructions)
    z, step + increment

let firstZ = [ for a in starting -> findNextZ'' a 0L ]

let rec cycle node step =
    let node, step = findNextZ'' node step
    if step % lengthOfInstructions = 0L then step
    else
        cycle node step


// hmm
let cycles = firstZ |> map (fun (n, s) -> (cycle n s) - s) |> map ( fun x -> x / lengthOfInstructions )

let result = (lengthOfInstructions) * (cycles |> List.reduce ((*)))


