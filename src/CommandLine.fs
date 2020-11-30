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

let parse args =
    let rec parseRec (args: string list) (resultSoFar: CommandLineResult) =
        match args with
        | [] -> resultSoFar
        | "-d" :: xs ->
            match xs with
            | [] -> parseRec xs resultSoFar
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

                    parseRec xss newResult
                | _ -> parseRec xs resultSoFar
        | "-i" :: xs ->
            match xs with
            | [] -> parseRec xs resultSoFar
            | path :: xss ->
                let newResult =
                    match resultSoFar with
                    | InvalidCommand _ ->
                        ValidCommand
                            { Day = NoDay
                              FilePath = (FilePath path) }
                    | ValidCommand cmd -> ValidCommand { cmd with FilePath = FilePath path }

                parseRec xss newResult
        | _ :: xs -> parseRec xs resultSoFar

    parseRec args (InvalidCommand "Missing options")
