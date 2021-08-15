// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
module Program

open System
open Dynastream.Fit
open EncodedWorkout

// Define a function to construct a message to print

[<EntryPoint>]
let main argv =
    EncodedWorkout.createEncoding "Running 800m Repeats"
    |> EncodedWorkout.addStep
        { Duration = Distance 1000;
          Intensity = Warmup;
          Notes = None;
          Name = None }
    |> EncodedWorkout.addStep
        { Duration = Distance 800;
          Intensity = Interval;
          Notes = None;
          Name = None }
    |> EncodedWorkout.addStep
        { Duration = Distance 200;
          Intensity = Rest;
          Notes = None;
          Name = None }
    |> EncodedWorkout.addRepeat 1 5u
    |> EncodedWorkout.addStep
        { Duration = Distance 1000;
          Intensity = Cooldown;
          Notes = None;
          Name = None }
    |> EncodedWorkout.addStep
        { Duration = Time 300;
          Intensity = Interval;
          Notes = None;
          Name = Some "Some name" }
    |> EncodedWorkout.addRepeat 0 2u
    |> EncodedWorkout.dumpFile @"C:\Users\carlo\OneDrive\Documentos"

    printfn "Deu porra"
    0 // return an integer exit code
