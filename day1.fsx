#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines "1.txt"

let digitWords =
    [ "zero"
      "one"
      "two"
      "three"
      "four"
      "five"
      "six"
      "seven"
      "eight"
      "nine" ]

let tryMatchPartOne (s: string) =
    [ '0' .. '9' ] |> tryFindIndex (fun c -> s[0] = c)

let tryMatchPartTwo (s: string) =
    [ 0..9 ]
    |> tryFindIndex (fun i -> s.StartsWith(i.ToString()) || s.StartsWith(digitWords[i]))

let subStrings (line: string) =
    [ for i in 0 .. (length line) - 1 -> line[i..] ]

let twoDigits matchF line =
    let digits = subStrings line |> map matchF |> filter Option.isSome |> map Option.get
    (digits |> head) * 10 + (digits |> Seq.last)

let partOne: int = input |> map (twoDigits tryMatchPartOne) |> sum
let partTwo: int = input |> map (twoDigits tryMatchPartTwo) |> sum
