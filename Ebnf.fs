module Ebnf

open System
open System.IO

open Arafel.CodeDom

let scan line =
    seq {
        let mutable depth = 0
        let mutable lit = false
        let mutable index = 0

        for ch in line do
            match ch with
            | '\'' -> lit <- not lit
            | '(' -> depth <- depth + if lit then 0 else 1
            | ')' -> depth <- depth - if lit then 0 else 1
            | _ -> ()
            yield (index, ch, depth, lit)
            index <- index + 1
    }

type Multiplicity = ZeroOrOne | ZeroOrMore | OneOrMore

let showMultiplicity m =
    match m with
    | ZeroOrOne -> "?"
    | ZeroOrMore -> "*"
    | OneOrMore -> "+"

type EbnfExpr =
    | Choice of EbnfExpr list
    | Sequence of EbnfExpr list
    | Primary of Multiplicity * EbnfExpr
    | Parens of EbnfExpr
    | StringLiteral of string
    | NcName of string

let rec show expr =
    match expr with
    | Choice list -> list |> List.map show |> String.concat " | "
    | Sequence list -> list |> List.map show |> String.concat " "
    | Primary (m, e) -> (show e) + (showMultiplicity m)
    | Parens e -> "(" + (show e) + ")"
    | StringLiteral s -> "'" + s + "'"
    | NcName n -> n

let rec mergeNames (list:string list) =
    match list with
    | [] -> ""
    | [n] -> n
    | [n; m] -> 
        if n.StartsWith(m + " ") then n
        else if m.StartsWith(n + " ") then m
        else if n = m then n
        else raise (Exception "")
    | n::rest -> mergeNames [n; mergeNames rest]

let rec parseItem (s:string) =
    match s[s.Length - 1] with
    | '?' -> Primary (ZeroOrOne, parseItem (s.Substring(0, s.Length - 1)))
    | '*' -> Primary (ZeroOrMore, parseItem (s.Substring(0, s.Length - 1)))
    | '+' -> Primary (OneOrMore, parseItem (s.Substring(0, s.Length - 1)))
    | ')' -> Parens (parseExpr (s.Substring(1, s.Length - 2)))
    | '\'' -> StringLiteral (s.Substring(1, s.Length - 2))
    | _ -> NcName s

and parseSequence (s:string) =
    let list = scan s |> Seq.filter (fun (index, ch, depth, lit) -> ch = ' ' && depth = 0 && not lit)
    let mutable i = 0
    let mutable items = []
    for (index, _, pipe, _) in list do
        items <- if index > i then s.Substring(i, index - i) :: items else items
        i <- index + 1
    match items with
    | [] -> parseItem (s.Substring(i))
    | _ -> Sequence (s.Substring(i) :: items |> List.rev |> List.map parseItem)

and parseExpr (s:string) =
    let list = scan s |> Seq.filter (fun (index, ch, depth, lit) -> ch = '|' && depth = 0 && not lit)
    let mutable i = 0
    let mutable choices = []
    for (index, _, pipe, _) in list do
        choices <- if index > i + 1 then s.Substring(i, index - i - 1).Trim() :: choices else choices
        i <- index + 1
    let result = Choice (s.Substring(i).Trim() :: choices |> List.rev |> List.map parseSequence)
    match result with
    | Choice [e] -> e
    | c -> c

let parseProduction (s:string) =
    let pair = s.Split("::=", StringSplitOptions.TrimEntries)
    let expr = parseExpr (pair[1])
    (pair[0], expr)


let rec getParserEbnf parser =
    match parser with
    | ProductionP (_, _) -> "_"
    | TokenP s -> plain s
    | LiteralP s -> "'" + (plain s) + "'"
    | OutP (_, p) -> getParserEbnf p
    | CheckpointP p -> getParserEbnf p
    | OptionP p -> "(" + (getParserEbnf p) + ")?"
    | OptionListP p -> "(" + (getParserEbnf p) + ")?"
    | ListP p -> "(" + (getParserEbnf p) + ")*"
    | NonEmptyListP p -> "(" + (getParserEbnf p) + ")+"
    | AndP (p, q) -> getParserEbnf(p) + " " + getParserEbnf(q)
    | OrP (p, q) -> "(" + getParserEbnf(p) + " | " + getParserEbnf(q) + ")"
    | DelimitedP (d, p) -> getParserEbnf(p) + " (" + getParserEbnf(d) + " " + getParserEbnf(p) + ")*"
    | SurroundP (a, b, p) -> getParserEbnf(a) + " " + getParserEbnf(p) + " " + getParserEbnf(b)   

let getFieldEbnf (TupleField(_, primary, _, parser)) =
    match primary with
    | ProductionType name -> (getParserEbnf parser).Replace("_", name)
    | _ -> getParserEbnf parser

let getUcEbnf (UnionCase(_, fields)) =
    String.concat " " (List.map getFieldEbnf fields)

let getEbnf ucs =
    String.concat " | " (List.map getUcEbnf ucs)

let writeEbnf filename productions =
    use writer = File.CreateText(filename)
    writer.WriteLine "/* Use, for example, at https://www.bottlecaps.de/rr/ui */"
    writer.WriteLine ()

    for Production(name, ucs, _) in productions do
        writer.WriteLine $"{name} ::= {getEbnf ucs}"

let writeDotField pname (TupleField (_, primaryType, _, _)) =
    $"{pname} -> {primaryType}"

let writeDotCase pname (UnionCase (_, fields)) =
    Seq.map (writeDotField pname) fields

let writeDotProduction (Production (pname, ucs, _)) =
    Seq.map (writeDotCase pname) ucs
    |> Seq.concat

let writeDot filename productions =

    let edges = 
        Seq.map writeDotProduction productions
        |> Seq.concat
        |> Seq.distinct

    use writer = File.CreateText(filename)
    writer.WriteLine "digraph G {"
    writer.WriteLine "    rankdir=\"BT\""
    writer.WriteLine "    { rank=same; bool; bigint; string; }"

    for edge in edges do
        writer.WriteLine $"    {edge}"

    writer.WriteLine "}"