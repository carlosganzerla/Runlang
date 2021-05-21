module CommandParserTests

open FsUnit.Xunit
open Xunit
open Utils
open Time
open Distance
open Interval
open Manipulation
open RootList
open LangParserTests
open CommandParser

let workout = parse "10x(1km TR + 500m LE)"
let initState = workout |> RootList.create

let runCommand state = parseCommand state

let extraState =
    [ "1km TR"; "5km 4:50/km~4:00/km"; "10min FO" ]
    |> List.map parse
    |> List.fold RootList.add initState

[<Fact>]
let ``List command returns current list`` () =
    runCommand initState "list"
    |> ok
    |> should equal (Updated initState)

[<Fact>]
let ``New command returns New state`` () =
    runCommand initState "new" |> ok |> should equal New

[<Fact>]
let ``Join dot returns list with all intervals joined`` () =
    let expected =
        workout
        |> Manipulation.join None
        |> ok
        |> RootList.add initState

    runCommand initState "join ."
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Join by index duplicates top of list`` () =
    let expected = RootList.add initState workout

    runCommand initState "join 6"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Join by range joins only the input indexes minus 1`` () =
    let expected =
        workout
        |> Manipulation.join (Some (2, 6))
        |> ok
        |> RootList.add initState

    runCommand initState "join 3-7"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Join by index out of range must return an error`` () =
    runCommand initState "join 23" |> shouldBeError

[<Fact>]
let ``Join by range outside of bounds must return an error`` () =
    runCommand initState "join 3-21" |> shouldBeError

[<Fact>]
let ``Joining with -r switch joins the root manipulation`` () =
    let expected =
        workout
        |> Manipulation.join None
        |> ok
        |> RootList.add extraState

    runCommand extraState "join . -r"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Joining with -m switch joins the selected manipulation index`` () =
    let workout = RootList.get 2 extraState |> ok
    let expected =
        workout
        |> Manipulation.join (Some (0, 2))
        |> ok
        |> RootList.add extraState

    runCommand extraState "join 1-3 -m 2"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Joining with -m switch with index out range must return error`` () =
    runCommand initState "join -m 2" |> shouldBeError

[<Fact>]
let ``Splitting by time with dot splits the whole manipulation`` () =
    let split = Time.create 0u 3u 0u |> ok |> TimeSplit
    let expected =
        workout
        |> Manipulation.split split None
        |> ok
        |> RootList.add initState
    runCommand initState "split 3:00 ."
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Splitting by distance with dot splits the whole manipulation`` () =
    let split = Meters 250u |> DistanceSplit
    let expected =
        workout
        |> Manipulation.split split None
        |> ok
        |> RootList.add initState
    runCommand initState "split 250m  ."
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Splitting by index just splits the index minus one of the interval`` () =
    let split = Kilometers 1.3m |> DistanceSplit
    let expected =
        workout
        |> Manipulation.split split (Some (3,3))
        |> ok
        |> RootList.add initState
    runCommand initState "split 1,3km 4"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Splitting by range splits the range minus one of the intervals`` () =
    let split = Time.create 0u 0u 35u |> ok |> TimeSplit
    let expected =
        workout
        |> Manipulation.split split (Some (1,15))
        |> ok
        |> RootList.add initState
    runCommand initState "split 35s 2-16"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Splitting by outside of bounds index return error`` () =
    runCommand initState "split 1s 25" |> shouldBeError

[<Fact>]
let ``Splitting by outside of bounds range return error`` () =
    runCommand initState "split 1s 15-21" |> shouldBeError
