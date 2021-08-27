module Args

open CommandLine
open StringUtils


type CliArguments =
    { [<Option('o',
               "open",
               Required = false,
               HelpText = "Export on open distance mode")>]
      OpenDistance: bool;
      [<Option('d',
               "display",
               Required = false,
               HelpText = "Display workout tree")>]
      DisplayTree: bool;
      [<Option("table", Required = false, HelpText = "Pace table path")>]
      PaceTablePath: string option;
      [<Option('n', "name", Required = false, HelpText = "Workout name")>]
      WorkoutName: string option;
      [<Option('p', "path", Required = false, HelpText = "Export path")>]
      WorkoutPath: string option;
      [<Option("no-file",
               Required = false,
               HelpText = "Do not generate workout file")>]
      NoExport: bool;
      [<Value(0,
              MetaName = "workout",
              Required = true,
              HelpText = "Workout string")>]
      WorkoutString: string }

let parseArgs argv =
    let parser = Parser.Default
    let parse = parser.ParseArguments

    argv
    |> parse
    |> function
    | :? (Parsed<CliArguments>) as parsed -> Some parsed.Value
    | _ -> None
