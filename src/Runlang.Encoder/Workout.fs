module Workout

open System.IO
open Dynastream.Fit

type WorkoutIntensity =
    | Active
    | Warmup
    | Cooldown
    | Recovery
    | Interval
    | Rest

type WorkoutDuration =
    | Time of int
    | Distance of int
    | Open

type WorkoutStep =
    { Duration: WorkoutDuration;
      Intensity: WorkoutIntensity;
      Notes: string option;
      Name: string option }

type WorkoutEncoding =
    private
        { FileId: FileIdMesg;
          Workout: WorkoutMesg;
          WorkoutSteps: WorkoutStepMesg list }

[<RequireQualifiedAccessAttribute>]
module Workout =

    let private createFileIdMessage () =
        let fileId = FileIdMesg ()
        let rand = System.Random ()
        fileId.SetType File.Workout
        fileId.SetManufacturer Manufacturer.Development
        fileId.SetProduct 0us
        fileId.SetTimeCreated (DateTime (System.DateTime.UtcNow))
        fileId.SetSerialNumber (rand.Next () |> uint)
        fileId

    let private createWorkoutMessage (name: string) =
        let workout = WorkoutMesg ()
        workout.SetSport Sport.Running
        workout.SetSubSport SubSport.Invalid
        workout.SetWktName (name.Replace (" ", "_"))
        workout

    let createEncoding workoutName =
        { Workout = createWorkoutMessage workoutName;
          FileId = createFileIdMessage ();
          WorkoutSteps = [] }

    let private fitIntensity intensity =
        match intensity with
        | Active -> Intensity.Active
        | Warmup -> Intensity.Warmup
        | Cooldown -> Intensity.Cooldown
        | Recovery -> Intensity.Recovery
        | Interval -> Intensity.Interval
        | Rest -> Intensity.Rest

    let private fitDuration duration =
        match duration with
        | Time seconds ->
            (seconds * 1000 |> abs |> uint |> Some, WktStepDuration.Time)
        | Distance meters ->
            (meters * 100 |> abs |> uint |> Some, WktStepDuration.Distance)
        | Open -> (None, WktStepDuration.Open)

    let private createStep encoding =
        let newStep = WorkoutStepMesg ()
        let steps = encoding.WorkoutSteps
        steps |> List.length |> uint16 |> newStep.SetMessageIndex
        (newStep, { encoding with WorkoutSteps = newStep :: steps })

    let addStep step encoding =
        let (newStep, encoding) = createStep encoding
        let (durationValue, durationType) = fitDuration step.Duration
        let intensity = fitIntensity step.Intensity
        step.Name |> Option.iter newStep.SetWktStepName
        step.Notes |> Option.iter newStep.SetNotes
        newStep.SetDurationType durationType
        durationValue |> Option.iter newStep.SetDurationValue
        newStep.SetIntensity intensity
        newStep.SetTargetType WktStepTarget.Invalid
        encoding

    let addRepeat (fromStep: int) (count: int) encoding =
        let (newStep, encoding) = createStep encoding
        newStep.SetDurationType (WktStepDuration.RepeatUntilStepsCmplt)
        newStep.SetDurationValue (fromStep |> abs |> uint)
        newStep.SetTargetType (WktStepTarget.Open)
        newStep.SetTargetValue (count |> abs |> uint)
        encoding


    let dumpFile directory encoding =
        encoding.Workout.SetNumValidSteps (
            encoding.WorkoutSteps |> List.length |> uint16
        )

        let fileName =
            Path.Combine (
                directory,
                $"{encoding.Workout.GetWktNameAsString ()}.fit"
            )

        use fs =
            new FileStream (
                fileName,
                FileMode.Create,
                FileAccess.ReadWrite,
                FileShare.Read
            )

        let encoder = Encode ProtocolVersion.V10
        encoder.Open fs
        encoder.Write encoding.FileId
        encoder.Write encoding.Workout
        encoding.WorkoutSteps |> List.rev |> List.iter encoder.Write
        encoder.Close ()
        fs.Close ()
