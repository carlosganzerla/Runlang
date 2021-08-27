module StringUtils

open System
open System.IO

let inline join sep (strings: string seq) =
    String.Join (sep, Seq.filter (String.IsNullOrEmpty >> not) strings)

let inline trim (str: string) = str.Trim ()

let inline split (sep: string) (str: string) = str.Split sep

let inline toUpper (str: string) = str.ToUpperInvariant ()

let inline contains (text: string) (str: string) = str.Contains text

let inline appendPath (path: string) (root: string) = Path.Combine (root, path)

let inline str (obj: obj) = obj.ToString ()
