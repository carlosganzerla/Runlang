module ParserTests

open FsUnit.Xunit
open Xunit
open Pace
open Parser
open Interval

let paceTable = function
    | MAX -> Pace.createOrThrow 03u 0u
    | FTS -> Pace.createOrThrow 04u 0u
    | FO -> Pace.createOrThrow 05u 0u
    | MO -> Pace.createOrThrow 06u 0u
    | LE -> Pace.createOrThrow 07u 0u
    | LVS -> Pace.createOrThrow 08u 0u
    | TR -> Pace.createOrThrow 09u 0u
    | CV -> Pace.createOrThrow 10u 0u
    | CA -> Pace.createOrThrow 11u 0u
    | CL -> Pace.createOrThrow 12u 0u

let parse = parseWorkout paceTable

let parseOk = parseWorkout paceTable >> function
    | Ok intervals -> Interval.listToString intervals
    | Error err-> raise (System.Exception err)

[<Fact>]
let ``Distance and term pace interval in km`` () =
    let input = "1km MO"
    let expected = ["#1 Time: 00:06:00, Distance: 1.00km, Pace: 6:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Distance and time pace interval in km`` () =
    let input = "1km 4:40/km"
    let expected = ["#1 Time: 00:04:40, Distance: 1.00km, Pace: 4:40/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Distance and time pace interval in m`` () =
    let input = "500m 4:40/km"
    let expected = ["#1 Time: 00:02:20, Distance: 500m, Pace: 4:40/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Numeric time and pace only seconds`` () =
    let input = "30s 2:30/km"
    let expected = ["#1 Time: 00:00:30, Distance: 200m, Pace: 2:30/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Numeric time and pace only minutes`` () =
    let input = "4min 4:00/km"
    let expected = ["#1 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Numeric time and pace only hours`` () =
    let input = "1h 5:00/km"
    let expected = ["#1 Time: 01:00:00, Distance: 12.00km, Pace: 5:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Numeric time and pace minutes and seconds`` () =
    let input = "3min30s 3:30/km"
    let expected = ["#1 Time: 00:03:30, Distance: 1.00km, Pace: 3:30/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Numeric time and pace hours and minutes`` () =
    let input = "1h04min 4:00/km"
    let expected = ["#1 Time: 01:04:00, Distance: 16.00km, Pace: 4:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Numeric time and pace hours and seconds`` () =
    let input = "1h30s 2:00/km"
    let expected = ["#1 Time: 01:00:30, Distance: 30.25km, Pace: 2:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Numeric time and pace hours, minutes and seconds`` () =
    let input = "1h30min30s FO"
    let expected = ["#1 Time: 01:30:30, Distance: 18.10km, Pace: 5:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Watch time minutes and pace`` () =
    let input = "5:00 5:00/km"
    let expected = ["#1 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Watch time hours and pace`` () =
    let input = "01:24:24 4:00/km"
    let expected = ["#1 Time: 01:24:24, Distance: 21.10km, Pace: 4:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Time and distance interval`` () =
    let input = "32:00 8km"
    let expected = ["#1 Time: 00:32:00, Distance: 8.00km, Pace: 4:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Progression intervals`` () =
    let input = "10km 4:40/km~3:55/km"
    let expected = [
        "#1 Time: 00:04:40, Distance: 1.00km, Pace: 4:40/km";
        "#2 Time: 00:04:35, Distance: 1.00km, Pace: 4:35/km";
        "#3 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km";
        "#4 Time: 00:04:25, Distance: 1.00km, Pace: 4:25/km";
        "#5 Time: 00:04:20, Distance: 1.00km, Pace: 4:20/km";
        "#6 Time: 00:04:15, Distance: 1.00km, Pace: 4:15/km";
        "#7 Time: 00:04:10, Distance: 1.00km, Pace: 4:10/km";
        "#8 Time: 00:04:05, Distance: 1.00km, Pace: 4:05/km";
        "#9 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#10 Time: 00:03:55, Distance: 1.00km, Pace: 3:55/km";
    ]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Sum intervals`` () =
    let input = "1km FO / 12min MO + 5min 1km"
    let expected = [
        "#1 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
        "#2 Time: 00:12:00, Distance: 2.00km, Pace: 6:00/km";
        "#3 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
    ]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Repetition intervals`` () =
    let input = "4x (400m FO + 100m CV)"
    let expected = [
        "#1 Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
        "#2 Time: 00:01:00, Distance: 100m, Pace: 10:00/km";
        "#3 Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
        "#4 Time: 00:01:00, Distance: 100m, Pace: 10:00/km";
        "#5 Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
        "#6 Time: 00:01:00, Distance: 100m, Pace: 10:00/km";
        "#7 Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
        "#8 Time: 00:01:00, Distance: 100m, Pace: 10:00/km";
    ]
    input |> parseOk |> should equal expected


[<Fact>]
let ``Sample workout warmup plus progression`` () =
    let input = "1km TR + 10km 4:40/km~3:55/km"
    let expected = [
        "#1 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
        "#2 Time: 00:04:40, Distance: 1.00km, Pace: 4:40/km";
        "#3 Time: 00:04:35, Distance: 1.00km, Pace: 4:35/km";
        "#4 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km";
        "#5 Time: 00:04:25, Distance: 1.00km, Pace: 4:25/km";
        "#6 Time: 00:04:20, Distance: 1.00km, Pace: 4:20/km";
        "#7 Time: 00:04:15, Distance: 1.00km, Pace: 4:15/km";
        "#8 Time: 00:04:10, Distance: 1.00km, Pace: 4:10/km";
        "#9 Time: 00:04:05, Distance: 1.00km, Pace: 4:05/km";
        "#10 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#11 Time: 00:03:55, Distance: 1.00km, Pace: 3:55/km";
    ]
    input |> parseOk |> should equal expected


[<Fact>]
let ``Sample workout warmup plus repetitions plus cooldown`` () =
    let input = "2km TR / 6x(1km FTS + 2min CV) / 2km LE"
    let expected = [
        "#1 Time: 00:18:00, Distance: 2.00km, Pace: 9:00/km";
        "#2 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#3 Time: 00:02:00, Distance: 200m, Pace: 10:00/km";
        "#4 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#5 Time: 00:02:00, Distance: 200m, Pace: 10:00/km";
        "#6 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#7 Time: 00:02:00, Distance: 200m, Pace: 10:00/km";
        "#8 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#9 Time: 00:02:00, Distance: 200m, Pace: 10:00/km";
        "#10 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#11 Time: 00:02:00, Distance: 200m, Pace: 10:00/km";
        "#12 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#13 Time: 00:02:00, Distance: 200m, Pace: 10:00/km";
        "#14 Time: 00:14:00, Distance: 2.00km, Pace: 7:00/km";
    ]
    input |> parseOk |> should equal expected

