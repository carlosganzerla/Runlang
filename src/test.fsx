open System.IO
open System

let inline toUpper (str: string) = str.ToUpperInvariant()

let inline contains (text: string) (str: string) = str.Contains text

let appendPath (path: string) (root: string) = Path.Combine(root, path)

let copy src dest = File.Copy(src, dest)

let getGarmin () =
    DriveInfo.GetDrives()
    |> Array.filter
        (fun (drive: DriveInfo) ->
            drive.IsReady
            && drive.VolumeLabel |> toUpper |> contains "GARMIN")
    |> Array.map (fun (drive: DriveInfo) -> drive.RootDirectory.FullName)
    |> Array.toList
    |> List.tryHead
    |> Option.map (appendPath "GARMIN/NEWFILES")


let test () =
    getGarmin ()
    |> Option.map (appendPath "test.fit")
    |> Option.iter (copy "/home/carlo/Documents/teste_xport.fit")

printfn "%A" (Environment.GetFolderPath Environment.SpecialFolder.Personal)
