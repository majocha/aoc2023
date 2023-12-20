#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "20.txt"

type Module =
    | Broadcaster
    | FlipFlop
    | Conjunction

let getName (str: string) =
    match str[0], str[1..] with
    | '%', name -> name
    | '&', name -> name
    | _ -> str

let getType (str: string) =
    match str[0] with
    | '%' -> FlipFlop
    | '&' -> Conjunction
    | _ -> Broadcaster

let parse input =
    input
    |> Seq.map (sscanf "%s -> %s")
    |> Seq.map (fun (m, cs) -> m, cs |> String.split [ ", " ] |> Seq.toList)
    |> Seq.toList

let configuration = parse input

let getInputs n =
    configuration
    |> List.filter (fun (m, cs) -> cs |> Seq.contains n)
    |> List.map (fst >> getName)

type Pulse = Pulse of sender: string * bool

let broadcaster send n dests =
    function
    | (Pulse(_, q)) -> Pulse(n, q) |> send dests

let flipflop send n dests =
    let mutable state = false

    function
    | Pulse(_, false) ->
        state <- not state
        Pulse(n, state) |> send dests
    | _ -> id

let conjunction send n dests inps =
    let mutable received = inps |> List.map (fun n -> n, false) |> Map

    fun (Pulse(from, q)) ->
        received <- received.Add(from, q)
        let q = received.Values |> Seq.forall id |> not
        Pulse(n, q) |> send dests

let makeModule send (n, dests: string list) =
    let k = getName n

    match getType n with
    | Broadcaster -> k, broadcaster send k dests
    | FlipFlop -> k, flipflop send k dests
    | Conjunction -> k, getInputs k |> conjunction send k dests

module PartOne =
    let push n =

        let send dests (Pulse(sender, q) as p) queue = queue @ [ for d in dests -> d, p ]

        let modules = configuration |> List.map (makeModule send) |> Map

        let rec processQueue low high =
            function
            | [] -> low, high
            | (dest, (Pulse(n, q) as p)) :: queue ->
                let low, high = if q then low, high + 1 else low + 1, high
                let queue =
                    if modules.ContainsKey dest then
                        modules[dest] p queue
                    else
                        queue

                processQueue low high queue


        let l, h =
            [ 1..n ]
            |> List.fold (fun (h, l) _ -> processQueue h l [ "broadcaster", Pulse("button", false) ]) (0, 0)

        l * h

    let partOne = push 1000

let partTwo =
    let push n =
        let send dests (Pulse(sender, q) as p) queue = queue @ [ for d in dests -> d, p ]

        let modules = configuration |> List.map (makeModule send) |> Map

        let rec processQueue bp sqs =
            function
            | [] -> sqs
            | (dest, (Pulse(n, q) as p)) :: queue ->
                let sqs =
                    if q && dest = "sq" then (n, bp) :: sqs else sqs

                let queue =
                    if modules.ContainsKey dest then
                        modules[dest] p queue
                    else
                        queue

                processQueue bp sqs queue


        [ 1..n ]
        |> List.fold (fun sqs bp -> processQueue bp sqs [ "broadcaster", Pulse("button", false) ]) []

    // This is not a general solution:
    let inputs = [ "fv"; "kk"; "vt"; "xr" ]
    let pushes = push 20000

    [ for x in inputs do
          pushes
          |> List.filter (fst >> (=) x)
          |> List.map snd
          |> List.pairwise
          |> List.map (fun (x1, x2) -> x1 - x2)
          |> List.head
          |> int64 ]
    |> List.reduce ((*))
