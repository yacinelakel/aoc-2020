module CommandLineParser 
    type DayOption = | NoDay | Day of int
    type InputOption = |NoInput | Input of string

    type CommandLineOptions = {
        Day: DayOption;
        Input: InputOption; 
    }

    type CommandLineResult = 
        | ValidCommand of CommandLineOptions
        | InvalidCommand

    let parse args = 
        let rec parseRec (args:string list) (resultSoFar:CommandLineResult) = 
            match args with
            | [] -> resultSoFar
            | "-d"::xs -> 
                match xs with 
                |[] -> 
                    parseRec xs resultSoFar
                |dayStr::xss ->
                    match System.Int32.TryParse dayStr with
                    |true, day ->
                        let newResult = 
                            match resultSoFar with
                            | InvalidCommand -> 
                                ValidCommand {Day=Day (int day); Input=NoInput}
                            | ValidCommand cmd ->
                                ValidCommand {cmd with Day = Day (int day)}
                        parseRec xss newResult
                    |_ -> parseRec xs resultSoFar
            | "-i"::xs ->
                match xs with 
                |[] -> 
                    parseRec xs resultSoFar
                |filename::xss ->
                    let newResult = 
                        match resultSoFar with
                        | InvalidCommand -> 
                            ValidCommand {Day=NoDay; Input=(Input filename)}
                        | ValidCommand cmd ->
                            ValidCommand {cmd with Input = Input filename}
                    parseRec xss newResult
            | _::xs -> 
                parseRec xs resultSoFar

        parseRec args InvalidCommand
