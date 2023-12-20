#time "on"
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "20.txt"

type Module = Broadcaster | FlipFlop  | Conjunction

let getName (str: string) =
    match str[0], str[1..] with | '%', name -> name | '&', name -> name | _ -> str

let getType (str: string) =
   match str[0] with | '%' -> FlipFlop | '&' -> Conjunction  | _ -> Broadcaster

let parse input =
    input |> Seq.map (sscanf "%s -> %s")
    |> Seq.map (fun (m, cs) -> m, cs |> String.split [", "] |> Seq.toList)
    |> Seq.toList

let configuration = parse input

let getInputs n =
    configuration |> List.filter (fun (m, cs) -> cs |> Seq.contains n ) |> List.map (fst >> getName)

type Pulse = Pulse of sender: string * bool

let broadcaster send n dests = function (Pulse(_, q)) -> Pulse(n, q) |> send dests

let flipflop send n dests =
    let mutable state = false
    function Pulse(_, false) -> state <- not state; Pulse(n, state) |> send dests | _ -> id

let conjunction send n dests inps =
    let mutable received = inps |> List.map (fun n -> n, false) |> Map
    fun (Pulse(from, q)) ->
        received <- received.Add(from, q)
        let q = received.Values |> Seq.forall id |> not
        Pulse(n, q) |> send dests

let makeModule send (n, dests: string list) =
    let k = getName n
    match getType n with
    | Broadcaster -> k, broadcaster send  k dests
    | FlipFlop -> k, flipflop send  k dests
    | Conjunction -> k, getInputs k |> conjunction send k dests 

let push n =

    let send dests (Pulse(sender, q) as p) queue =
        queue @ [ for d in dests -> d, p ]
    
    let modules = configuration |> List.map (makeModule send) |> Map
    
    let rec processQueue low high =
        function
        | [] -> low, high
        | (dest, (Pulse(n, q) as p)) :: queue ->
            let v = if q then "-high" else "-low"
            printfn $"{n} {v}-> {dest}"
            let high, low = if q then high + 1, low else high, low + 1
            let queue =
                if dest = "output" then queue
                else
                    modules[dest] p queue
            processQueue high low queue


    [1 .. n] |> List.fold (fun (h, l) _ -> processQueue h l [ "broadcaster", Pulse("button", false) ] ) (0, 0)

push 1


