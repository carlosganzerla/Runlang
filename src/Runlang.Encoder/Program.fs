// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Dynastream.Fit
open Workout

// Define a function to construct a message to print

[<EntryPoint>]
let main argv =
    Workout.createEncoding "Running 800m Repeats"
    |> Workout.addStep { Duration = Distance 1000; Intensity = Warmup; Notes = None; Name = None }
    |> Workout.addStep { Duration = Distance 800; Intensity = Interval; Notes = None; Name = None; }
    |> Workout.addStep { Duration = Distance 200; Intensity = Rest; Notes = None; Name = None }
    |> Workout.addRepeat 1 5
    |> Workout.addStep { Duration = Distance 1000; Intensity = Cooldown; Notes = None; Name = None }
    |> Workout.addStep { Duration = Time 300; Intensity = Interval; Notes = None; Name = Some "Some name" }
    |> Workout.addRepeat 0 2
    |> Workout.dumpFile @"C:\Users\carlo\OneDrive\Documentos"
    printfn "Deu porra" 
    0 // return an integer exit code
