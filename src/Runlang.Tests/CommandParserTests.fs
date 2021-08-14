module CommandParserTests

open FsUnit.Xunit
open Xunit
open Utils
open Time
open Distance
open Interval
open Manipulation
open RootList
open CommandParser

let workout = parseToIntervals "10x(1km TR + 500m LE)"
let initState = workout |> RootList.create

let runCommand state = parseCommand state

let extraState =
    [ "1km TR"; "5km 4:50/km->4:00/km:1km"; "10min FO" ]
    |> List.map parseToIntervals
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
    let split = Time.create 0 3 0 |> ok |> TimeSplit

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
    let split = Distance.meters 250 |> DistanceSplit

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
    let split = Distance.kilometers 1.3m |> DistanceSplit

    let expected =
        workout
        |> Manipulation.split split (Some (3, 3))
        |> ok
        |> RootList.add initState

    runCommand initState "split 1,3km 4"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Splitting by range splits the range minus one of the intervals`` () =
    let split = Time.create 0 0 35 |> ok |> TimeSplit

    let expected =
        workout
        |> Manipulation.split split (Some (1, 15))
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

[<Fact>]
let ``Split with -r switch splits the root manipulation`` () =
    let split = Time.create 0 1 20 |> ok |> TimeSplit

    let expected =
        workout
        |> Manipulation.split split (Some (2, 5))
        |> ok
        |> RootList.add extraState

    runCommand extraState "split 00:01:20 3-6 -r"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Split with -m switch splits the selected manipulation`` () =
    let workout = RootList.get 1 extraState |> ok
    let split = Time.create 1 0 5 |> ok |> TimeSplit

    let expected =
        workout
        |> Manipulation.split split None
        |> ok
        |> RootList.add extraState

    runCommand extraState "split 1h05s . -m 1"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Split with -m switch outside of bounds returns an error`` () =
    runCommand initState "split 1m . -m 3" |> shouldBeError

[<Fact>]
let ``Copy dot duplicates the list`` () =
    let expected = RootList.copyRange None extraState |> ok

    runCommand extraState "cp ."
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Copy range duplicates selected elements to the top`` () =
    let expected = RootList.copyRange (Some (0, 2)) extraState |> ok

    runCommand extraState "cp 0-2"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Copy index duplicates selected element to the top`` () =
    let expected = RootList.copyRange (Some (0, 0)) extraState |> ok

    runCommand extraState "cp 0"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Copy index out of bounds returns an error`` () =
    runCommand initState "cp 1" |> shouldBeError

[<Fact>]
let ``Copy range out of bounds returns an error`` () =
    runCommand initState "cp 1-3" |> shouldBeError

[<Fact>]
let ``Move dot is a no op`` () =
    runCommand extraState "mv ."
    |> ok
    |> should equal (Updated extraState)

[<Fact>]
let ``Move range moves selected elements to the top`` () =
    let expected = RootList.moveRange (Some (1, 3)) extraState |> ok

    runCommand extraState "mv 1-3"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Move index moves selected element to the top`` () =
    let expected = RootList.moveRange (Some (2, 2)) extraState |> ok

    runCommand extraState "mv 2"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Move root index should return an error`` () =
    runCommand initState "mv 0" |> shouldBeError

[<Fact>]
let ``Move range within root should return an error`` () =
    runCommand extraState "mv 0-2" |> shouldBeError

[<Fact>]
let ``Move index out of bounds returns an error`` () =
    runCommand initState "mv 1" |> shouldBeError

[<Fact>]
let ``Move range out of bounds returns an error`` () =
    runCommand initState "mv 1-3" |> shouldBeError

[<Fact>]
let ``Remove dot clears list except for root`` () =
    runCommand extraState "rm ."
    |> ok
    |> should equal (Updated initState)

[<Fact>]
let ``Remove range removes selected elements`` () =
    let expected = RootList.removeRange (Some (1, 3)) extraState |> ok

    runCommand extraState "rm 1-3"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Remove index removes selected element`` () =
    let expected = RootList.removeRange (Some (2, 2)) extraState |> ok

    runCommand extraState "rm 2"
    |> ok
    |> should equal (Updated expected)

[<Fact>]
let ``Remove root index should return an error`` () =
    runCommand initState "rm 0" |> shouldBeError

[<Fact>]
let ``Remove range within root should return an error`` () =
    runCommand extraState "rm 0-2" |> shouldBeError

[<Fact>]
let ``Remove index out of bounds returns an error`` () =
    runCommand initState "rm 1" |> shouldBeError

[<Fact>]
let ``Remove range out of bounds returns an error`` () =
    runCommand initState "rm 1-3" |> shouldBeError
