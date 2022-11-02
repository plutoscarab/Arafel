module Ebnf

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
            yield (index, ch, depth)
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

let rec parseItem (s:string) =
    match s[s.Length - 1] with
    | '?' -> Primary (ZeroOrOne, parseItem (s.Substring(0, s.Length - 1)))
    | '*' -> Primary (ZeroOrMore, parseItem (s.Substring(0, s.Length - 1)))
    | '+' -> Primary (OneOrMore, parseItem (s.Substring(0, s.Length - 1)))
    | ')' -> Parens (parseExpr (s.Substring(1, s.Length - 2)))
    | '\'' -> StringLiteral (s.Substring(1, s.Length - 2))
    | _ -> NcName s

and parseSequence (s:string) =
    let list = scan s |> Seq.filter (fun (index, ch, depth) -> ch = ' ' && depth = 0)
    let mutable i = 0
    let mutable items = []
    for (index, _, pipe) in list do
        items <- if index > i then s.Substring(i, index - i) :: items else items
        i <- index + 1
    match items with
    | [] -> parseItem (s.Substring(i))
    | _ -> Sequence (s.Substring(i) :: items |> List.rev |> List.map parseItem)

and parseExpr (s:string) =
    let list = scan s |> Seq.filter (fun (index, ch, depth) -> ch = '|' && depth = 0)
    let mutable i = 0
    let mutable choices = []
    for (index, _, pipe) in list do
        choices <- if index > i + 1 then s.Substring(i, index - i - 1).Trim() :: choices else choices
        i <- index + 1
    match choices with
    | [] -> parseSequence (s.Substring(i).Trim())
    | _ -> Choice (s.Substring(i).Trim() :: choices |> List.rev |> List.map parseSequence)
