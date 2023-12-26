#time "on"
#r "nuget: FSharpPlus"
#r "nuget: MathNet.Numerics.FSharp"

open FSharpPlus

open MathNet.Numerics.LinearAlgebra

let input = System.IO.File.ReadAllLines "24.txt"
let d = 1f
let parseLine line =
    let pstr, vstr = sscanf "%s @ %s" line
    let parseVector str = str |> String.split [ ", "] |> List.ofSeq |> List.map float
    parseVector pstr |> vector, parseVector vstr |> vector

let segments = input |> Seq.map parseLine |> Seq.toList

let p = segments |> List.map fst
let v = segments |> List.map snd

let p1, p2, p3 = p[1], p[2], p[3]
let v1, v2, v3 = v[1], v[2], v[3]








module PartOne = 
    open System.Numerics
    let d1, d2 = 2f , 4f 

    let intersection (x1, x2) (x3, x4) =
        let a = x2 - x1
        let b = x4 - x3
        let c = x3 - x1
        let x = x1 + a * Vector3.Dot(Vector3.Cross(c, b), Vector3.Cross(a, b)) / Vector3.Cross(a, b).LengthSquared()
        x

    let twoPoints (x,v) = x, v + x

    let belongs (x: Vector3) ((x1: Vector3), (v1: Vector3)) ((x2: Vector3), (v2: Vector3)) =
        ( (x.X < x1.X && v1.X < 0f) || (x.X > x1.X && v1.X > 0f)) &&
        ( (x.X < x2.X && v2.X < 0f) || (x.X > x2.X && v2.X > 0f)) &&
        x.X >= d1 && x.X <= d2 && x.Y >= d1 && x.Y <= d2

    let allPairs =
        [
            for i in 0 .. segments.Length - 2 do
                for j in i + 1 .. segments.Length - 1 do 
                    segments[i], segments[j] ]

    let intersectsInside (s1, s2) =
        let l1, l2 = twoPoints s1, twoPoints s2
        let x = intersection l1 l2
        belongs x s1 s2

    intersectsInside (segments[0], segments[1])

    let partOne = allPairs |> List.filter intersectsInside |> List.length



