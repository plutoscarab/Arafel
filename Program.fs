open System
open System.CodeDom.Compiler
open System.Globalization
open System.IO
open System.Linq
open System.Numerics
open System.Text
open Microsoft.FSharp.Reflection
open Language

type PrimaryType =
    | StringType
    | BigintType
    | ProductionType of string

type Multiplicity =
    | SingleM
    | OptionM
    | ListM

type Parser =
    | ProductionP
    | TokenP of string
    | LiteralP of string
    | OptionP of Parser
    | OptionListP of Parser
    | ListP of Parser
    | NonEmptyListP of Parser
    | AndP of Parser * Parser
    | OrP of Parser * Parser
    | DelimitedP of Parser * Parser
    | SurroundP of Parser * Parser * Parser

type TupleField =
    | TupleField of PrimaryType * Multiplicity * Parser

type UnionCase =
    | UnionCase of string * TupleField list

type Production =
    | Production of string * UnionCase list


let rec fsharpAbbv (t:Type) =
    if t.Name = "FSharpList`1" then
        t.GetGenericArguments().[0] |> fun f -> (fsharpAbbv f) + " list"
    elif t.Name = "FSharpOption`1" then
        (fsharpAbbv (t.GetGenericArguments().[0])) + " option"
    elif t.IsGenericType then
        t.Name + "<" + (t.GetGenericArguments() |> List.ofArray |> List.map fsharpAbbv |> String.concat ", ") + ">"
    elif t.Name = "String" then
        "string"
    elif t.Name = "BigInteger" then
        "bigint"
    else
        t.Name

let fsharpName (t:Type) =
    if FSharpType.IsUnion(t) then
        FSharpType.GetUnionCases(t)
        |> Array.map (fun c ->
            let fields = c.GetFields()
            if fields = [||] then
                c.Name
            else
                c.Name + " of " + (
                    fields
                    |> Array.map (fun f -> fsharpAbbv f.PropertyType)
                    |> String.concat " * "))
        |> String.concat " | "
    else if FSharpType.IsTuple(t) then
        FSharpType.GetTupleElements(t)
        |> Array.map fsharpAbbv
        |> String.concat " * "
    else 
        fsharpAbbv t

let getImmediateDependencies (t:Type) =
    let deps =
        if FSharpType.IsTuple(t) then
            FSharpType.GetTupleElements(t) 
            |> Seq.ofArray
        elif FSharpType.IsUnion(t) then
            FSharpType.GetUnionCases(t) 
            |> Seq.ofArray
            |> Seq.map (fun c -> c.GetFields() |> Seq.ofArray |> Seq.map (fun f -> f.PropertyType))
            |> Seq.concat
        elif t.IsGenericType then
            t.GetGenericArguments() 
            |> Seq.ofArray
        else
            Seq.empty
    deps 
    |> Seq.map (fun f ->
        if f.Name = "FSharpList`1" || f.Name = "FSharpOption`1" then
            f.GetGenericArguments().[0]
        else
            f)
    |> Seq.distinct
    |> List.ofSeq

let rec decodeParser ps =
    match ps with
    | "_"::rest ->
        ProductionP, rest
    | "opt"::rest ->
        let (p, rest') = decodeParser rest
        (OptionP p), rest'
    | "opt[]"::rest ->
        let (p, rest') = decodeParser rest
        (OptionListP p), rest'
    | "0+"::rest ->
        let (p, rest') = decodeParser rest
        (ListP p, rest')
    | "1+"::rest ->
        let (p, rest') = decodeParser rest
        (NonEmptyListP p, rest')
    | "and"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        (AndP(p, q), rest'')
    | "or"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        (OrP(p, q), rest'')
    | "delim"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        (DelimitedP(p, q), rest'')
    | "surr"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        let (r, rest''') = decodeParser rest''
        (SurroundP(p, q, r), rest''')
    | s::rest when s.Length > 2 && s.StartsWith("'") && s.EndsWith("'") ->
        LiteralP(s.Substring(1, s.Length - 2)), rest
    | s::rest when s = s.ToUpperInvariant() ->
        TokenP(s), rest
    | _ ->
        raise (NotImplementedException())

let decodeParserStr (s:string) =
    let r, n = decodeParser (s.Split(' ') |> List.ofArray)
    if n <> [] then raise (Exception "")
    r

let primaryType (t:Type) =
    if t.Name = "String" then
        StringType
    elif t.Name = "BigInteger" then
        BigintType
    else
        ProductionType t.Name

let tupleField (t:Type) parser =
    if t.Name = "FSharpList`1" then
        TupleField(primaryType (t.GetGenericArguments().[0]), ListM, parser)
    elif t.Name = "FSharpOption`1" then
        TupleField(primaryType (t.GetGenericArguments().[0]), OptionM, parser)
    else
        TupleField(primaryType t, SingleM, parser)

let tupleFields (c:UnionCaseInfo) =
    let fields = c.GetFields()
    let n = Array.length fields

    let parsers =
        c.GetCustomAttributes()
        |> Array.filter (fun f -> f :? Parse.ParseAttribute)
        |> Array.map (fun f -> f :?> Parse.ParseAttribute)
        |> Array.map (fun f -> decodeParserStr f.Syntax)

    if n <> Array.length(parsers) then raise (Exception "")
    Array.zip fields parsers
    |> Array.map (fun (f, p) -> tupleField f.PropertyType p)
    |> List.ofArray

let unionCases (t:Type) =
    FSharpType.GetUnionCases(t)
    |> List.ofArray
    |> List.map (fun c ->
        UnionCase(c.Name, tupleFields c))

let rec getReferencedTypes pending result =
    match pending with
    | [] -> result
    | t::rest ->
        if (List.contains t result) then
            getReferencedTypes rest result
        else
            getReferencedTypes ((getImmediateDependencies t) @ rest) (t::result)

let getProductions (t:Type) =
    let moduleName = t.FullName.Substring(0, t.FullName.IndexOf('+') + 1)
    getReferencedTypes [t] []
    |> List.filter (fun f -> f.FullName.StartsWith(moduleName))
    |> List.map (fun t -> Production(t.Name, unionCases t))

let rec getParserEbnf parser =
    match parser with
    | ProductionP -> "_"
    | TokenP(s) -> s
    | LiteralP(s) -> "'" + s + "'"
    | OptionP(p) -> "(" + (getParserEbnf p) + ")?"
    | OptionListP(p) -> "(" + (getParserEbnf p) + ")?"
    | ListP(p) -> "(" + (getParserEbnf p) + ")*"
    | NonEmptyListP(p) -> "(" + (getParserEbnf p) + ")+"
    | AndP(p, q) -> getParserEbnf(p) + " " + getParserEbnf(q)
    | OrP(p, q) -> "(" + getParserEbnf(p) + " | " + getParserEbnf(q) + ")"
    | DelimitedP(d, p) -> getParserEbnf(p) + " (" + getParserEbnf(d) + " " + getParserEbnf(p) + ")*"
    | SurroundP(a, b, p) -> getParserEbnf(a) + " " + getParserEbnf(p) + " " + getParserEbnf(b)   

let getFieldEbnf (TupleField(primary, _, parser)) =
    match primary with
    | ProductionType name -> (getParserEbnf parser).Replace("_", name)
    | _ -> getParserEbnf parser

let getUcEbnf (UnionCase(_, fields)) =
    String.concat " " (List.map getFieldEbnf fields)

let getEbnf ucs =
    String.concat " | " (List.map getUcEbnf ucs)

let writeEbnf filename productions =
    use writer = File.CreateText(filename)

    for Production(name, ucs) in productions do
        writer.WriteLine $"{name} ::= {getEbnf ucs}"

let rec writeParser (writer:IndentedTextWriter) parser primaryType =
    match parser with
    | ProductionP ->
        match primaryType with
        | ProductionType name -> writer.Write (name.ToLowerInvariant())
        | _ -> raise (NotImplementedException())
    | TokenP(s) ->
        let tokenType =
            match primaryType with
            | StringType -> "stringToken"
            | BigintType -> "bigintToken"
            | _ -> raise (NotImplementedException())
        let tokenCtor = s.Substring(0, 1) + s.Substring(1).ToLowerInvariant()
        writer.Write $"{tokenType} {tokenCtor}"
    | LiteralP(s) ->
        writer.Write $"literal \"{s}\""
    | OptionP(p) ->
        writer.Write "option ("
        writeParser writer p primaryType
        writer.Write ")"
    | OptionListP(p) ->
        writer.Write "optionlist ("
        writeParser writer p primaryType
        writer.Write ")"
    | ListP(p) ->
        writer.Write "zeroOrMore ("
        writeParser writer p primaryType
        writer.Write ")"
    | NonEmptyListP(p) ->
        writer.Write "oneOrMore ("
        writeParser writer p primaryType
        writer.Write ")"
    | AndP(p, q) ->
        writer.Write "andThen ("
        writeParser writer p primaryType
        writer.Write ") ("
        writeParser writer q primaryType
        writer.Write ")"
    | OrP(p, q) ->
        writer.Write "orElse ("
        writeParser writer p primaryType
        writer.Write ") ("
        writeParser writer q primaryType
        writer.Write ")"
    | DelimitedP(d, p) ->
        writer.Write "delimited ("
        writeParser writer d primaryType
        writer.Write ") ("
        writeParser writer p primaryType
        writer.Write ")"
    | SurroundP(a, b, p) ->
        writer.Write "surround ("
        writeParser writer a primaryType
        writer.Write ") ("
        writeParser writer b primaryType
        writer.Write ") ("
        writeParser writer p primaryType
        writer.Write ")"

let writeField (writer:IndentedTextWriter) (TupleField(primaryType, _, parser)) =
    writeParser writer parser primaryType
    writer.WriteLine ()

let writeCase (writer:IndentedTextWriter) (UnionCase(name, fields)) =
    writer.WriteLine "parser {"
    writer.Indent <- writer.Indent + 1
    let mutable i = 0

    for field in fields do
        writer.Write $"let! f{i} = "
        writeField writer field
        i <- i + 1

    let fs = String.concat ", " (List.map (fun f -> $"f{f}") [0..i-1])
    writer.WriteLine $"return {name}({fs})"
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "}"

let writeParserFile filename modulename (productions:Production list) =
    use file = File.CreateText(filename)
    use writer = new IndentedTextWriter(file)
    writer.WriteLine $"module {modulename}"
    writer.WriteLine ()
    writer.WriteLine "open Language"
    writer.WriteLine "open Lexer"
    writer.WriteLine "open Parse"
    let mutable keyword = "let rec"

    for Production(name, cases) in productions do
        writer.WriteLine ()
        let pname = name.ToLowerInvariant()
        writer.WriteLine $"{keyword} {pname} tokens ="
        keyword <- "and"
        writer.Indent <- writer.Indent + 1

        if List.length(cases) = 1 then
            writer.Write "let p = "
            writeCase writer cases.[0]
            writer.WriteLine "p tokens"
        else
            writer.WriteLine "let p = parser {"
            writer.Indent <- writer.Indent + 1

            for unionCase in cases do
                writer.Write "return! "
                writeCase writer unionCase

            writer.Indent <- writer.Indent - 1
            writer.WriteLine "}"
            writer.WriteLine "p tokens"

        writer.Indent <- writer.Indent - 1

let main =

    Console.OutputEncoding <- Encoding.Unicode
    (*
    let languageGrammar = buildTypes "language.txt" "Language.fs" "Language"
    let coreGrammar = readGrammar "core.grammar.txt"
    buildGrammar coreGrammar "Core.fs" "Core"
    let grammar = readGrammar "grammar.txt"
    buildGrammar grammar "Arafel.fs" "Arafel"

    let src = File.ReadAllText("sample.af")
    let cursor = Lexer.makeCursor src
    let tokens = Lexer.tokenise cursor |> Seq.toArray
    let mutable tc = { Lexer.TokenCursor.source = tokens; Lexer.TokenCursor.index = 0 }

    while tc.More do
        let result = Arafel.expr tc
        match result with
        | (Parse.Error, _) -> raise (Exception $"failed at token {tc.index}")
        | (tree, next) ->
            Console.WriteLine ()
            dump tree "" true
            let syntax = syntaxTree tree
            tc <- next
    *)

    let productions = getProductions typeof<Expr>
    writeEbnf "language.txt" productions
    writeParserFile "Arafel.fs" "Arafel" productions

    let src = File.ReadAllText("sample.af")
    let runes = src.EnumerateRunes() |> Seq.toList
    let cursors = Cursor.getCursors runes |> Seq.toArray
    let cursor = Cursor.makeCursor src
    let tokens = Lexer.tokenise cursor |> Seq.toList

    let mutable t = tokens

    while t <> [] do
        let (r, t2) = Arafel.expr t
        if r = None then raise (Exception "")
        t <- t2

    0