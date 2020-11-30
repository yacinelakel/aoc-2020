namespace Days.Types

type PartResult =
    | NotImplemented
    | Answer of string

type DayResult = PartResult * PartResult

module Defaults =
    let NotImplementedResult = (NotImplemented, NotImplemented)
