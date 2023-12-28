#time "on"
#r "nuget: FSharpPlus"
#r "nuget: MathNet.Numerics.FSharp"

open FSharpPlus

open MathNet.Numerics.LinearAlgebra

let input = System.IO.File.ReadAllLines "24.txt"

let parseLine line =
    let pstr, vstr = sscanf "%s @ %s" line

    let parseVector str =
        str |> String.split [ ", " ] |> List.ofSeq |> List.map float

    parseVector pstr |> vector, parseVector vstr |> vector

let p, v = input |> Seq.map parseLine |> Seq.toList |> List.unzip

module PartOne =
    open System.Numerics

    type V = MathNet.Numerics.LinearAlgebra.Vector<float>

    let intersection (x1, x2) (x3, x4) =
        let a = x2 - x1
        let b = x4 - x3
        let c = x3 - x1

        let x =
            x1
            + a * Vector3.Dot(Vector3.Cross(c, b), Vector3.Cross(a, b))
              / Vector3.Cross(a, b).LengthSquared()

        x |> tap (printfn "%A")

    let zz (x: V) = Vector3(single x[0], single x[1], 0f)

    let d = 100000000000000f

    let belongs (x: Vector3) ((x1: Vector3), (v1: Vector3)) ((x2: Vector3), (v2: Vector3)) =
        let d1, d2 = 2f * d, 4f * d

        ((x.X < x1.X && v1.X < 0f) || (x.X > x1.X && v1.X > 0f))
        && ((x.X < x2.X && v2.X < 0f) || (x.X > x2.X && v2.X > 0f))
        && x.X >= d1
        && x.X <= d2
        && x.Y >= d1
        && x.Y <= d2

    let allPairs =
        [ for i in 0 .. input.Length - 2 do
              for j in i + 1 .. input.Length - 1 do
                  (zz p[i], zz v[i]), (zz p[j], zz v[j]) ]

    let intersectsInside (s1, s2) =
        let l (p: Vector3, v) =
            // scale down, because of float32 precision 
            let p = p / d
            p, p + v

        let x = intersection (l s1) (l s2)
        belongs (x * d) s1 s2

    let partOne = allPairs |> List.filter intersectsInside |> List.length

let p1, p2, p3 = p[0], p[1], p[2]
let v1, v2, v3 = v[0], v[1], v[2]

let cross (a: _ Vector) (b: _ Vector) =
    let a1, a2, a3 = a[0], a[1], a[2]
    let b1, b2, b3 = b[0], b[1], b[2]
    vector
      [ a2 * b3 - a3 * b2
        a3 * b1 - a1 * b3
        a1 * b2 - a2 * b1 ]

let crossMatrix (x: _ Vector) =
    let x1, x2, x3 = x[0], x[1], x[2]
    matrix
      [ [ 0.; -x3; x2 ]
        [ x3; 0.; -x1 ]
        [ -x2; x1; 0. ] ]

let b =
    vector
      [ yield! (cross p2 v2) - (cross p1 v1)
        yield! (cross p3 v3) - (cross p1 v1) ]

let M =
    matrix
        [ yield!
              (crossMatrix v1 - crossMatrix v2)
                  .Append(crossMatrix p2 - crossMatrix p1)
                  .EnumerateRows()
          yield!
              (crossMatrix v1 - crossMatrix v3)
                  .Append(crossMatrix p3 - crossMatrix p1)
                  .EnumerateRows() ]

let partOne = PartOne.partOne
let partTwo = M.Solve b |> Seq.take 3 |> Seq.sum |> sprintf "%.0f"
