module Arafel.CodeDom

type Formatter =
    | Raw
    | Newline
    | Indent
    | Outdent
    | Space

type Parser =
    | ProductionP of Formatter * Formatter
    | TokenP of string
    | LiteralP of string
    | OptionP of Parser
    | OptionListP of Parser
    | ListP of Parser
    | NonEmptyListP of Parser
    | CheckpointP of Parser
    | OutP of string * Parser
    | AndP of Parser * Parser
    | OrP of Parser * Parser
    | DelimitedP of Parser * Parser
    | SurroundP of Parser * Parser * Parser

type PrimaryType =
    | StringType
    | BigintType
    | BoolType
    | ProductionType of string
    with
    override this.ToString() =
        match this with
        | StringType -> "string"
        | BigintType -> "bigint"
        | BoolType -> "bool"
        | ProductionType p -> p

type Multiplicity =
    | SingleM
    | OptionM
    | ListM

type TupleField =
    | TupleField of string * PrimaryType * Multiplicity * Parser

type UnionCase =
    | UnionCase of string * TupleField list

type Production =
    | Production of string * UnionCase list * bool

let rec isNonempty =
    function
    | NonEmptyListP _ -> true
    | DelimitedP _ -> true
    | AndP (_, p) -> isNonempty p
    | SurroundP (_, _, p) -> isNonempty p
    | CheckpointP p -> isNonempty p
    | _ -> false

let plain (s: string) =
    s.Replace("␠", "")
     .Replace("␤", "")
     .Replace("␏", "")
     .Replace("␎", "")
     .Replace("␑", "")
     .Replace("␅", "")
