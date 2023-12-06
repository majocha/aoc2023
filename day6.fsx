#time

let records = [ 7, 9; 15, 40; 30, 200 ]

let distance timeToHold time =
    let speed = timeToHold
    (time - timeToHold) * speed

let races allowedTime =
    [ for tth in 0L .. allowedTime -> distance tth allowedTime ]

let winning t dist =
    races t |> List.where (fun d -> d > dist) |> List.length

let partOne = [ for t, d in records -> winning t d ] |> List.reduce (*)

let partTwo = winning 71530L 940200L
