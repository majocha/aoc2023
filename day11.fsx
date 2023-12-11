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

let distance' g1 g2 d =
    let s1, s2 = dim d g1, dim d g2
    let s = abs (s2 - s1)

    if s = 0 then
        0L
    else
        let emptyLines =
            [ s1 .. sign (s2 - s1) .. s2 ] |> List.filter (isLineEmpty d) |> length

        int64 (999_999 * emptyLines + s)

let distance (g1, g2) =
    distance' g1 g2 Row + distance' g1 g2 Col

let allPairs (gs: _ list) =
    [ let S = gs.Length - 1

      for i in 0..S do
          for j in i + 1 .. S do
              gs[i], gs[j] ]

galaxies |> allPairs |> map distance |> sum
