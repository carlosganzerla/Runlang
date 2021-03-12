module Test

open System
open System.Text.RegularExpressions
open System.Globalization

let culture = CultureInfo.InvariantCulture
let hregex = "([0-9]+h)?"
let minregex = "([0-5]?[0-9]min)?"
let sregex = "([0-5]?[0-9]s)?"
let hminsregex = hregex + minregex + sregex
let timeRegex = "([0-9]+):([0-5][0-9]):([0-5][0-9])|([0-5]?[0-9]):([0-5][0-9])"

let parse time =
    let r = new Regex(hminsregex)
    let m = r.Match time
    let groups =
        m.Groups
        |> Seq.skip 1
        |> Seq.toList
        |> List.map (fun x -> if x.Value = "" then 0u else uint x.Value)
    printf "%A" groups

//parse "1:33:33"
