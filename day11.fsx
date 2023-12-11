#time
#r "nuget: FSharpPlus"

open FSharpPlus

let input = System.IO.File.ReadAllLines("11.txt")

let SIZE = input[0] |> length

let galaxies =
    [ for y in 0 .. SIZE - 1 do
          for x in 0 .. SIZE - 1 do
              if input[y][x] = '#' then
                  x, y ]

type Dim =
    | Row
    | Col

let dim =
    function
    | Row -> fst
    | Col -> snd

let isLineEmpty =
    fun d n -> galaxies |> filter (fun g -> g |> dim d = n) = []
    |> memoizeN

let distance' e g1 g2 d =
    let s1, s2 = dim d g1, dim d g2
    let s1, s2 = min s1 s2, max s1 s2
    let emptyLines = [ s1..s2 ] |> filter (isLineEmpty d) |> length
    int64 (e * emptyLines + s2 - s1)

let distance e (g1, g2) =
    distance' e g1 g2 Row + distance' e g1 g2 Col

let allPairs (gs: _ list) =
    [ let S = gs.Length - 1

      for i in 0..S do
          for j in i + 1 .. S do
              gs[i], gs[j] ]

let partOne = galaxies |> allPairs |> map (distance 1) |> sum
let partTwo = galaxies |> allPairs |> map (distance 999_999) |> sum
