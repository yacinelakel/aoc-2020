module CommandLineParser
type DayOption =
    | NoDay
    | Day of int

type FilePathOption =
    | NoFilePath
    | FilePath of string

type CommandLineOptions =
    { Day: DayOption
      FilePath: FilePathOption }

type CommandLineResult =
    | ValidCommand of CommandLineOptions
    | InvalidCommand of string

let parseCommandLine args =
    let rec parseCommandLineRec (args: string list) (resultSoFar: CommandLineResult) =
        match args with
        | [] -> resultSoFar
        | "-d" :: xs ->
            match xs with
            | [] -> parseCommandLineRec xs resultSoFar
            | dayStr :: xss ->
                match System.Int32.TryParse dayStr with
                | true, day ->
                    let newResult =
                        match resultSoFar with
                        | InvalidCommand _ ->
                            ValidCommand
                                { Day = Day(int day)
                                  FilePath = NoFilePath }
                        | ValidCommand cmd -> ValidCommand { cmd with Day = Day(int day) }

                    parseCommandLineRec xss newResult
                | _ -> parseCommandLineRec xs resultSoFar
        | "-i" :: xs ->
            match xs with
            | [] -> parseCommandLineRec xs resultSoFar
            | path :: xss ->
                let newResult =
                    match resultSoFar with
                    | InvalidCommand _ ->
                        ValidCommand
                            { Day = NoDay
                              FilePath = (FilePath path) }
                    | ValidCommand cmd -> ValidCommand { cmd with FilePath = FilePath path }

                parseCommandLineRec xss newResult
        | _ :: xs -> parseCommandLineRec xs resultSoFar

    parseCommandLineRec args (InvalidCommand "Missing options")
