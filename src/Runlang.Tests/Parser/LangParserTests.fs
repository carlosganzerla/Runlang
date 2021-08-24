module LangParserTests

open FsUnit.Xunit
open Xunit
open Utils
open LangParser
open WorkoutTree

let parseToString = parseWorkout >> ok >> WorkoutTree.toString

[<Fact>]
let ``Distance and term pace step in km`` () =
    let input = "1km MO"
    let expected = "1.00km MO"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Distance and time pace step in km`` () =
    let input = "1.5km 4:40/km"
    let expected = "1.50km 4:40/km"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Distance and time pace step in m`` () =
    let input = "500m 4:40/km"
    let expected = input
    input |> parseToString |> should equal expected

[<Fact>]
let ``Numeric time and pace step only seconds`` () =
    let input = "30s 2:30/km"
    let expected = "00:00:30 2:30/km"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Numeric time and pace step only minutes`` () =
    let input = "4min 4:00/km"
    let expected = "00:04:00 4:00/km"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Numeric time and pace step only hours`` () =
    let input = "1h 5:00/km"
    let expected = "01:00:00 5:00/km"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Numeric time and pace minutes and seconds`` () =
    let input = "3min30s 3:30/km"
    let expected = "00:03:30 3:30/km"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Numeric time and pace hours and minutes`` () =
    let input = "1h04min 4:00/km"
    let expected = "01:04:00 4:00/km"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Numeric time and pace hours step and seconds`` () =
    let input = "1h30s 2:00/km"
    let expected = "01:00:30 2:00/km"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Numeric time and pace hours, minutes and seconds`` () =
    let input = "1h30min30s FO"
    let expected = "01:30:30 FO"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Watch time minutes and pace`` () =
    let input = "5:00 5:00/km"
    let expected = "00:05:00 5:00/km"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Watch time hours and pace step`` () =
    let input = "01:24:24 4:00/km"
    let expected = input
    input |> parseToString |> should equal expected

[<Fact>]
let ``Time and distance interval step`` () =
    let input = "32:00 8,2km"
    let expected = "00:32:00 8.20km"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Progression steps`` () =
    let input = "10.00km 4:40/km->3:55/km:1.00km"
    let expected = input
    input |> parseToString |> should equal expected

[<Fact>]
let ``Sum steps`` () =
    let input = "1km FO / 12min MO + 5min 1km"
    let expected = "1.00km FO + 00:12:00 MO + 00:05:00 1.00km"

    input |> parseToString |> should equal expected

[<Fact>]
let ``Repeat workout`` () =
    let input = "4x(400m FO + 100m CV)"
    let expected = input
    input |> parseToString |> should equal expected

[<Fact>]
let ``Nested Repeat workout`` () =
    let input = "1km TR + 8x(1km LE + 2x(200m MAX + 200m CL))"
    let expected = "1.00km TR + 8x(1.00km LE + 2x(200m MAX + 200m CL))"
    input |> parseToString |> should equal expected

[<Fact>]
let ``Highly complex workout`` () =
    let input =
        "20m TR + 3x(2x(20m FO + 1m FTS) + 4x(1m FO + 1m TR) + 1m TR + 1m LE)"

    let expected = input

    input |> parseToString |> should equal expected

[<Fact>]
let ``Sample workout warmup plus progression`` () =
    let input = "1km TR / 10km TR->FTS:1500m"

    let expected = "1.00km TR + 10.00km TR->FTS:1500m"

    input |> parseToString |> should equal expected

[<Fact>]
let ``Sample workout warmup plus repetitions plus cooldown`` () =
    let input = "2km TR / 6x(1km FTS + 2min CV) / 2km LE"

    let expected = "2.00km TR + 6x(1.00km FTS + 00:02:00 CV) + 2.00km LE"
    input |> parseToString |> should equal expected
