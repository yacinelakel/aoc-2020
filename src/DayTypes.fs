namespace Days.Types

type PartResult =
    | NotImplemented
    | Answer of int

type DayResult = PartResult * PartResult

module Defaults =
    let NotImplementedResult = (NotImplemented, NotImplemented)
