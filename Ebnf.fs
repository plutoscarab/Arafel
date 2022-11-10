module Ebnf

open System

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
    | Choice of (EbnfExpr * string option) list
    | Fields of EbnfExpr list
    | Sequence of EbnfExpr list
    | Primary of Multiplicity * EbnfExpr
    | Parens of EbnfExpr
    | StringLiteral of string
    | NcName of string

let rec show expr =
    match expr with
    | Choice list -> list |> List.map (fun c -> show (fst c)) |> String.concat " | "
    | Fields list -> list |> List.map show |> String.concat " * "
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

let rec getName expr =
    match expr with
    | Choice list -> list |> List.map (fun c -> getName (fst c)) |> Seq.distinct |> Seq.filter (fun s -> s <> "") |> Seq.toList |> mergeNames
    | Fields list | Sequence list -> list |> List.map getName |> Seq.distinct |> Seq.filter (fun s -> s <> "") |> Seq.toList |> mergeNames
    | Primary (m, e) ->
        let name = getName e
        match m with
        | ZeroOrOne -> if not (name.Contains(' ')) then name + " option" else name
        | _ -> if (name.EndsWith(" option")) then name.Replace(" option", " list") else if name.Contains(' ') then name else name + " list"
    | Parens e -> getName e
    | StringLiteral s -> ""
    | NcName n -> 
        match n with
        | "NAT" -> "bigint"
        | name when name = name.ToUpperInvariant() -> "string"
        | _ -> n

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

and parseFields (s:string) =
    let list = scan s |> Seq.filter (fun (index, ch, depth, lit) -> ch = '⊗' && depth = 0 && not lit)
    let mutable i = 0
    let mutable items = []
    for (index, _, pipe, _) in list do
        items <- if index > i then s.Substring(i, index - i).Trim() :: items else items
        i <- index + 1
    match items with
    | [] -> parseSequence (s.Substring(i).Trim())
    | _ -> Fields (s.Substring(i).Trim() :: items |> List.rev |> List.map parseSequence)

and parseChoice (seq:string) =
    let quote1 = seq.IndexOf('‹')
    let quote2 = seq.IndexOf('›')
    let seqName = if quote1 = -1 then None else Some (seq.Substring(quote1 + 1, quote2 - quote1 - 1))
    let s = if quote1 = -1 then seq else seq.Substring(0, quote1).Trim()
    (parseFields s, seqName)

and parseExpr (s:string) =
    let list = scan s |> Seq.filter (fun (index, ch, depth, lit) -> ch = '|' && depth = 0 && not lit)
    let mutable i = 0
    let mutable choices = []
    for (index, _, pipe, _) in list do
        choices <- if index > i + 1 then s.Substring(i, index - i - 1).Trim() :: choices else choices
        i <- index + 1
    match choices with
    | [] -> parseFields (s.Substring(i).Trim())
    | _ -> Choice (s.Substring(i).Trim() :: choices |> List.rev |> List.map parseChoice)

let parseProduction (s:string) =
    let pair = s.Split("::=", StringSplitOptions.TrimEntries)
    let expr = parseExpr (pair[1])
    (pair[0], expr)
