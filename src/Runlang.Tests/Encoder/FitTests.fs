module EncoderExtensionTests

open FsUnit.Xunit
open Utils
open Xunit
open FitWorkout
open FitEncoder

[<Fact>]
let ``Encode workout tree generates a device step list`` () =
    let expected =
        [ FitWorkoutStep.createDefault (Distance 1000) Active "TR";
          FitWorkoutStep.createDefault (Distance 400) Interval "FO";
          FitWorkoutStep.createDefault (Distance 100) Rest "CL";
          FitWorkoutStep.createRepeat 1 3u;
          FitWorkoutStep.createDefault (Distance 1000) Active "LE" ]

    let encoded =
        parseOk "1km TR + 3x(400m FO + 100m CL) + 1km LE"
        |> WorkoutTree.toFit Default

    encoded |> should equal expected

[<Fact>]
let ``Encode workout generate a list with dots representing progression`` () =
    let expected =
        [ FitWorkoutStep.createDefault (Distance 1000) Active "TR.....FTS";
          FitWorkoutStep.createDefault (Distance 1000) Active "....FTS";
          FitWorkoutStep.createDefault (Distance 1000) Active "...FTS";
          FitWorkoutStep.createDefault (Distance 1000) Active "...FTS";
          FitWorkoutStep.createDefault (Distance 1000) Active "..FTS";
          FitWorkoutStep.createDefault (Distance 1000) Active ".FTS";
          FitWorkoutStep.createDefault (Distance 1000) Active ".FTS";
          FitWorkoutStep.createDefault (Distance 1000) Active "FTS" ]

    let encoded = parseOk "8km TR->FTS:1km" |> WorkoutTree.toFit Default

    encoded |> should equal expected

[<Fact>]
let ``Deeply nested short with repeats referring to normal steps`` () =
    let expected =
        [ FitWorkoutStep.createDefault (Time 60) Rest "CL";
          FitWorkoutStep.createDefault (Time 60) Active "4:40/km";
          FitWorkoutStep.createDefault (Distance 30) Interval "FO";
          FitWorkoutStep.createDefault (Time 300) Active "1.00km";
          FitWorkoutStep.createRepeat 3 3u;
          FitWorkoutStep.createDefault (Distance 1000) Active "TR";
          FitWorkoutStep.createRepeat 2 5u;
          FitWorkoutStep.createDefault (Distance 1000) Active "LE";
          FitWorkoutStep.createRepeat 0 2u;
          FitWorkoutStep.createDefault (Time 60) Interval "FTS" ]

    let encoded =
        parseOk
            """2x(1min CL + 1min 4:40/km + 5x(30m FO +
    3x(5min 1km) + 1km TR) + 1km LE) + 1min FTS"""
        |> WorkoutTree.toFit Default

    encoded |> should equal expected

[<Fact>]
let ``Step list on OpenDistance mode sets duration on name`` () =
    let expected =
        [ FitWorkoutStep.createDefault Open Active "1.00km TR";
          FitWorkoutStep.createDefault Open Interval "400m FO";
          FitWorkoutStep.createDefault Open Rest "100m CL";
          FitWorkoutStep.createRepeat 1 3u;
          FitWorkoutStep.createDefault Open Active "1.00km LE" ]

    let encoded =
        parseOk "1km TR + 3x(400m FO + 100m CL) + 1km LE"
        |> WorkoutTree.toFit OpenDistance

    encoded |> should equal expected

[<Fact>]
let ``Step list on OpenDistance progression contains each step distance`` () =
    let expected =
        [ FitWorkoutStep.createDefault Open Active "1.00km TR.....FTS";
          FitWorkoutStep.createDefault Open Active "1.00km ....FTS";
          FitWorkoutStep.createDefault Open Active "1.00km ...FTS";
          FitWorkoutStep.createDefault Open Active "1.00km ...FTS";
          FitWorkoutStep.createDefault Open Active "1.00km ..FTS";
          FitWorkoutStep.createDefault Open Active "1.00km ..FTS";
          FitWorkoutStep.createDefault Open Active "1.00km .FTS";
          FitWorkoutStep.createDefault Open Active "1.00km .FTS";
          FitWorkoutStep.createDefault Open Active "300m FTS" ]

    let encoded =
        parseOk "8.3km TR->FTS:1km"
        |> WorkoutTree.toFit OpenDistance

    encoded |> should equal expected

[<Fact>]
let ``Deeply nested workout on OpenDistance mode`` () =
    let expected =
        [ FitWorkoutStep.createDefault (Time 60) Rest "CL";
          FitWorkoutStep.createDefault (Time 60) Active "4:40/km";
          FitWorkoutStep.createDefault Open Interval "30m FO";
          FitWorkoutStep.createDefault (Time 300) Active "1.00km";
          FitWorkoutStep.createRepeat 3 3u;
          FitWorkoutStep.createDefault Open Active "1.00km TR";
          FitWorkoutStep.createRepeat 2 5u;
          FitWorkoutStep.createDefault Open Active "1.00km LE";
          FitWorkoutStep.createRepeat 0 2u;
          FitWorkoutStep.createDefault (Time 60) Interval "FTS" ]

    let encoded =
        parseOk
            """2x(1min CL + 1min 4:40/km + 5x(30m FO +
    3x(5min 1km) + 1km TR) + 1km LE) + 1min FTS"""
        |> WorkoutTree.toFit OpenDistance

    encoded |> should equal expected
