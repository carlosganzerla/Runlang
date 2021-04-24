module ManipulationTests

open FsUnit.Xunit
open Utils
open Xunit
open ParserTests
open Interval
open Manipulation
open Utils

let intervals = parse "1km TR + 1km LVS + 1km LE + 1km MO + 1km FO + 1km FTS"

[<Fact>]
let ``Joining intervals outside manipulation or with wrong indexing must yield error`` () =
    let joins = [
        Manipulation.join intervals (Range (-1,5)) 
        Manipulation.join intervals (Range (1,7)) 
        Manipulation.join intervals (Range (-1,9)) 
        Manipulation.join intervals (Range (3,2)) 
    ]
    joins |> List.iter shouldBeError


[<Fact>]
let ``Joining the entire manipulation should result in a single interval`` () =
    let (Ok join) = Manipulation.join intervals EntireList
    let expected = ["#1 Time: 00:39:00, Distance: 6.00km, Pace: 6:30/km"]
    join |> Interval.listToString |> should equal expected


[<Fact>]
let ``Joining a range manipulation should covert it to a single interval`` () =
    let (Ok join) = Manipulation.join intervals (Range (1,3))
    let expected = [
        "#1 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
        "#2 Time: 00:21:00, Distance: 3.00km, Pace: 7:00/km";
        "#3 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
        "#4 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
    ]
    join |> Interval.listToString |> should equal expected
