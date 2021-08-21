module EncoderIo

open StringUtils
open System
open System.IO


let exportDirectory fileName =
    let homePath = Environment.GetFolderPath Environment.SpecialFolder.Personal

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
    |> Option.defaultValue homePath
    |> appendPath fileName
