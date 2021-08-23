module EncoderExtensionTests

open FsUnit.Xunit
open Utils
open Xunit
open EncodedWorkout
open EncoderExtensions

[<Fact>]
let ``Encode workout tree generates a device step list`` () =
    let expected =
        [ EncodedWorkoutStep.createDefault (Distance 1000) Active "TR";
          EncodedWorkoutStep.createDefault (Distance 400) Interval "FO";
          EncodedWorkoutStep.createDefault (Distance 100) Rest "CL";
          EncodedWorkoutStep.createRepeat 1 3u;
          EncodedWorkoutStep.createDefault (Distance 1000) Active "LE" ]

    let encoded =
        parseOk "1km TR + 3x(400m FO + 100m CL) + 1km LE"
        |> WorkoutTree.encode Default

    encoded |> should equal expected

[<Fact>]
let ``Encode workout generate a list with dots representing progression`` () =
    let expected =
        [ EncodedWorkoutStep.createDefault (Distance 1000) Active "TR.....FTS";
          EncodedWorkoutStep.createDefault (Distance 1000) Active "....FTS";
          EncodedWorkoutStep.createDefault (Distance 1000) Active "...FTS";
          EncodedWorkoutStep.createDefault (Distance 1000) Active "...FTS";
          EncodedWorkoutStep.createDefault (Distance 1000) Active "..FTS";
          EncodedWorkoutStep.createDefault (Distance 1000) Active ".FTS";
          EncodedWorkoutStep.createDefault (Distance 1000) Active ".FTS";
          EncodedWorkoutStep.createDefault (Distance 1000) Active "FTS" ]

    let encoded = parseOk "8km TR->FTS:1km" |> WorkoutTree.encode Default

    encoded |> should equal expected

[<Fact>]
let ``Deeply nested short with repeats referring to normal steps`` () =
    let expected =
        [ EncodedWorkoutStep.createDefault (Time 60) Rest "CL";
          EncodedWorkoutStep.createDefault (Time 60) Active "4:40/km";
          EncodedWorkoutStep.createDefault (Distance 30) Interval "FO";
          EncodedWorkoutStep.createDefault (Time 300) Active "1.00km";
          EncodedWorkoutStep.createRepeat 3 3u;
          EncodedWorkoutStep.createDefault (Distance 1000) Active "TR";
          EncodedWorkoutStep.createRepeat 2 5u;
          EncodedWorkoutStep.createDefault (Distance 1000) Active "LE";
          EncodedWorkoutStep.createRepeat 0 2u;
          EncodedWorkoutStep.createDefault (Time 60) Interval "FTS" ]

    let encoded =
        parseOk
            """2x(1min CL + 1min 4:40/km + 5x(30m FO +
    3x(5min 1km) + 1km TR) + 1km LE) + 1min FTS"""
        |> WorkoutTree.encode Default

    encoded |> should equal expected

[<Fact>]
let ``Step list on OpenDistance mode sets duration on name`` () =
    let expected =
        [ EncodedWorkoutStep.createDefault Open Active "1.00km TR";
          EncodedWorkoutStep.createDefault Open Interval "400m FO";
          EncodedWorkoutStep.createDefault Open Rest "100m CL";
          EncodedWorkoutStep.createRepeat 1 3u;
          EncodedWorkoutStep.createDefault Open Active "1.00km LE" ]

    let encoded =
        parseOk "1km TR + 3x(400m FO + 100m CL) + 1km LE"
        |> WorkoutTree.encode OpenDistance

    encoded |> should equal expected

[<Fact>]
let ``Step list on OpenDistance progression contains each step distance`` () =
    let expected =
        [ EncodedWorkoutStep.createDefault Open Active "1.00km TR.....FTS";
          EncodedWorkoutStep.createDefault Open Active "1.00km ....FTS";
          EncodedWorkoutStep.createDefault Open Active "1.00km ...FTS";
          EncodedWorkoutStep.createDefault Open Active "1.00km ...FTS";
          EncodedWorkoutStep.createDefault Open Active "1.00km ..FTS";
          EncodedWorkoutStep.createDefault Open Active "1.00km ..FTS";
          EncodedWorkoutStep.createDefault Open Active "1.00km .FTS";
          EncodedWorkoutStep.createDefault Open Active "1.00km .FTS";
          EncodedWorkoutStep.createDefault Open Active "300m FTS" ]

    let encoded =
        parseOk "8.3km TR->FTS:1km"
        |> WorkoutTree.encode OpenDistance

    encoded |> should equal expected

[<Fact>]
let ``Deeply nested workout on OpenDistance mode`` () =
    let expected =
        [ EncodedWorkoutStep.createDefault (Time 60) Rest "CL";
          EncodedWorkoutStep.createDefault (Time 60) Active "4:40/km";
          EncodedWorkoutStep.createDefault Open Interval "30m FO";
          EncodedWorkoutStep.createDefault (Time 300) Active "1.00km";
          EncodedWorkoutStep.createRepeat 3 3u;
          EncodedWorkoutStep.createDefault Open Active "1.00km TR";
          EncodedWorkoutStep.createRepeat 2 5u;
          EncodedWorkoutStep.createDefault Open Active "1.00km LE";
          EncodedWorkoutStep.createRepeat 0 2u;
          EncodedWorkoutStep.createDefault (Time 60) Interval "FTS" ]

    let encoded =
        parseOk
            """2x(1min CL + 1min 4:40/km + 5x(30m FO +
    3x(5min 1km) + 1km TR) + 1km LE) + 1min FTS"""
        |> WorkoutTree.encode OpenDistance

    encoded |> should equal expected
