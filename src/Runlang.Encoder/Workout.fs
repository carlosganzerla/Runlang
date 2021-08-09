module Workout

open Dynastream.Fit

type Intensity = Active 

type Duration = Time of int | Distance of int

type WorkoutEncoding = private {
    FileId: FileIdMesg;
    Workout: WorkoutMesg;
    WorkoutSteps: WorkoutStepMesg list;
}

let private createFileIdMessage () =
    let fileId = FileIdMesg ()
    let rand = System.Random()
    fileId.SetType File.Workout
    fileId.SetManufacturer Manufacturer.Development
    fileId.SetProduct (rand.Next 65535 |> uint16)
    fileId.SetTimeCreated (DateTime (System.DateTime.UtcNow))
    fileId.SetSerialNumber (rand.Next ()|> uint)
    fileId

let private createWorkoutMessage (name: string) =
    let workout = WorkoutMesg ()
    workout.SetSport Sport.Running
    workout.SetSubSport SubSport.Invalid
    workout.SetWktName name
    workout

let createEncoding name = 
    {
        Workout = createWorkoutMessage name
        FileId = createFileIdMessage ()
        WorkoutSteps = []
}


