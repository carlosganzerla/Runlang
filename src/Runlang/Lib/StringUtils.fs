module StringUtils

open System
open System.IO

let inline join sep (str: string seq) = String.Join (sep, str)

let inline toUpper (str: string) = str.ToUpperInvariant ()

let inline contains (text: string) (str: string) = str.Contains text

let inline appendPath (path: string) (root: string) = Path.Combine (root, path)
