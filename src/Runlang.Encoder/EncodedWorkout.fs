module EncodedWorkout

open System.IO
open Dynastream.Fit

type EncodedWorkoutIntensity =
    | Active
    | Warmup
    | Cooldown
    | Recovery
    | Interval
    | Rest

type EncodedWorkoutDuration =
    | Time of int
    | Distance of int
    | Open

type EncodedWorkoutStep =
    private
    | Default of EncodedDefaultStep
    | Repeat of EncodedRepeatStep

and private EncodedDefaultStep =
    { Duration: EncodedWorkoutDuration;
      Intensity: EncodedWorkoutIntensity;
      Notes: string option;
      Name: string option }

and private EncodedRepeatStep = { FromIndex: int; Count: uint }

[<RequireQualifiedAccess>]
module EncodedWorkoutStep =
    let createRepeat fromIndex count =
        Repeat { FromIndex = fromIndex; Count = count }

    let createDefault duration intensity name =
        Default
            { Duration = duration;
              Intensity = intensity;
              Name = Some name;
              Notes = Some name }



type EncodedWorkout =
    private
        { FileId: FileIdMesg;
          Workout: WorkoutMesg;
          WorkoutSteps: WorkoutStepMesg list }

[<RequireQualifiedAccess>]
module EncodedWorkout =

    let private createFileIdMessage () =
        let fileId = FileIdMesg ()
        let rand = System.Random ()
        fileId.SetType File.Workout
        fileId.SetManufacturer Manufacturer.Development
        fileId.SetProduct (rand.Next 65535 |> uint16)
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

    let private addDefaultStep step encoding =
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

    let private addRepeatStep step encoding =
        let (newStep, encoding) = createStep encoding
        newStep.SetDurationType (WktStepDuration.RepeatUntilStepsCmplt)
        newStep.SetDurationValue (step.FromIndex |> abs |> uint)
        newStep.SetTargetType (WktStepTarget.Open)
        newStep.SetTargetValue step.Count
        encoding

    let addStep step encoding =
        match step with
        | Default step -> addDefaultStep step encoding
        | Repeat step -> addRepeatStep step encoding

    let dumpFile encoding path =
        encoding.Workout.SetNumValidSteps (
            encoding.WorkoutSteps |> List.length |> uint16
        )

        use fs =
            new FileStream (
                Path.ChangeExtension (path, ".fit"),
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
