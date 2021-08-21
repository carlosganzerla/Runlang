module EncoderCli

open System
open LangParser
open EncoderExtensions
open EncodedWorkout
open Functions
open StringUtils
open System.IO

let homePath = Environment.GetFolderPath Environment.SpecialFolder.Personal

let findGarmin () =
    let isGarminDrive (drive: DriveInfo) =
        drive.IsReady
        && drive.VolumeLabel |> toUpper |> contains "GARMIN"

    let exportDirectory (drive: DriveInfo) =
        appendPath "GARMIN/NEWFILES" drive.RootDirectory.FullName

    DriveInfo.GetDrives ()
    |> Array.filter isGarminDrive
    |> Array.map exportDirectory
    |> Array.filter Directory.Exists
    |> Array.tryHead

let read () = () |> Console.ReadLine |> trim

let rec readMandatory desc =
    printfn "%s: " desc
    |> read
    |> function
        | "" -> readMandatory desc
        | value -> value

let readOptional desc fallback =
    printfn "%s (%s): " desc fallback
    |> read
    |> function
        | "" -> fallback
        | value -> value

let downloadWorkout tree =
    let steps = WorkoutTree.encode tree
    let name = readMandatory "Enter workout name"

    let defaultPath =
        findGarmin ()
        |> Option.defaultValue homePath
        |> appendPath $"{name}.fit"

    let encoding =
        EncodedWorkout.createEncoding name
        |> List.fold (flip EncodedWorkout.addStep)
        <| steps

    readOptional "Enter file path" defaultPath
    |> EncodedWorkout.dumpFile encoding


let rec app () =
    readMandatory "Enter workout string"
    |> parseWorkout
    |> function
        | Ok tree -> downloadWorkout tree
        | Error error -> printfn "%s" error
    |> app
