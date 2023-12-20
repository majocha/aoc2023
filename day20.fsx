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
    function Pulse(_, false) -> state <- not state; Pulse(n, state) |> send dests | _ -> ()

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

let agent = MailboxProcessor.Start( fun inbox ->
    let send dests (Pulse(sender, q) as p) =
        for d in dests do
            let high, low = inbox.PostAndReply(fun rc -> d, p, rc)
            printfn $"{high} {low}"

    let modules = configuration |> List.map (makeModule send) |> Map

    let rec loop high low = async {
        let! dest, (Pulse(n, q) as p), rc = inbox.Receive()
        //let v = if q then "-high" else "-low"
        //printfn $"{n} {v}-> {dest}"

        let high, low = if q then high + 1, low else high, low + 1
        rc.Reply(high, low)
        if dest <> "output" then modules[dest] p
        return! loop high low
    }
    loop 0 0
)

let pushButton() =
    agent.PostAndReply(fun rc -> "broadcaster", Pulse("button", false), rc)

[for i in  1 .. 1000 ->    pushButton() ] |> List.last


