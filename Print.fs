module Print

open System
open System.CodeDom.Compiler
open System.Collections.Generic
open System.IO
open System.Text

open Cursor
open Lexer
open Tokens
open Parse

type IndentAttribute() = inherit System.Attribute()

let rec private writeParser (writer:IndentedTextWriter) parser primaryType name =
    match parser with
    | ProductionP ->
        match primaryType with
        | StringType
        | BigintType ->
            writer.WriteLine $"writer.Write {name}"
        | ProductionType p ->
            writer.WriteLine $"print{p} writer {name}"
    | ProductionLineP ->
        match primaryType with
        | StringType
        | BigintType ->
            writer.WriteLine $"writer.WriteLine {name}"
        | ProductionType p ->
            writer.WriteLine $"print{p} writer {name}"
            writer.WriteLine "writer.WriteLine ()"
    | LineProductionP ->
        writer.WriteLine "writer.WriteLine ()"
        match primaryType with
        | StringType
        | BigintType ->
            writer.WriteLine $"writer.Write {name}"
        | ProductionType p ->
            writer.WriteLine $"print{p} writer {name}"
    | IndentProductionP ->
        writer.WriteLine "writer.Indent <- writer.Indent + 1"
        match primaryType with
        | StringType
        | BigintType ->
            writer.WriteLine $"writer.WriteLine {name}"
        | ProductionType p ->
            writer.WriteLine $"print{p} writer {name}"
            writer.WriteLine "writer.WriteLine ()"
        writer.WriteLine "writer.Indent <- writer.Indent - 1"
    | TokenP(s) ->
        writer.WriteLine $"let s = {name}.ToString()"
        writer.WriteLine "if s.ToString().EndsWith(\"\\n\") then"
        writer.WriteLine $"    let s' = s.Substring(0, s.Length - 1)"
        writer.WriteLine $"    writer.WriteLine s'"
        writer.WriteLine "else"
        writer.WriteLine $"    writer.Write {name}"
    | LiteralP(s) ->
        let u = s.Replace("␠", " ")
                 .Replace("␏", "")
                 .Replace("␎", "")
                 .Split('␍')
        for i in [0..u.Length-1] do
            if i < u.Length - 1 then
                writer.WriteLine $"writer.WriteLine \"{u.[i]}\""
            else
                writer.WriteLine $"writer.Write \"{u.[i]}\""
        if s.EndsWith("␏") then
            writer.WriteLine "writer.Indent <- writer.Indent + 1"
            writer.WriteLine "writer.WriteLine ()"
    | OptionP(p) ->
        writer.WriteLine $"match {name} with"
        writer.WriteLine "| None -> ignore()"
        writer.WriteLine $"| Some {name}' ->"
        writer.Indent <- writer.Indent + 1
        writeParser writer p primaryType (name + "'")
        writer.Indent <- writer.Indent - 1
    | OptionListP(p) ->
        writer.WriteLine $"match {name} with"
        writer.WriteLine "| [] -> ignore()"
        writer.WriteLine $"| _ ->"
        writer.Indent <- writer.Indent + 1
        writeParser writer p primaryType name
        writer.Indent <- writer.Indent - 1
    | ListP(p)
    | NonEmptyListP(p) ->
        writer.WriteLine $"for {name}' in {name} do"
        writer.Indent <- writer.Indent + 1
        writeParser writer p primaryType (name + "'")
        writer.Indent <- writer.Indent - 1
    | AndP(p, q) ->
        writeParser writer p primaryType name
        writeParser writer q primaryType name
    | OrP(p, q) ->
        writeParser writer p primaryType name
    | DelimitedP(d, p) ->
        writer.WriteLine $"match {name} with"
        writer.WriteLine "| [] -> ignore()"
        writer.WriteLine $"| [{name}'] ->"
        writer.Indent <- writer.Indent + 1
        writeParser writer p primaryType (name + "'")
        writer.Indent <- writer.Indent - 1
        writer.WriteLine $"| {name}'::{name}'' ->"
        writer.Indent <- writer.Indent + 1
        writeParser writer p primaryType (name + "'")
        writer.WriteLine $"for {name}_ in {name}'' do"
        writer.Indent <- writer.Indent + 1
        writeParser writer d primaryType (name + "_")
        writeParser writer p primaryType (name + "_")
        writer.Indent <- writer.Indent - 2
    | SurroundP(a, b, p) ->
        writeParser writer a primaryType name
        writeParser writer p primaryType name
        writeParser writer b primaryType name

let private writeField (writer:IndentedTextWriter) (TupleField(primaryType, _, parser)) name =
    writeParser writer parser primaryType name

let private writeCase (writer:IndentedTextWriter) fields =
    writer.Indent <- writer.Indent + 1
    let mutable i = 0

    for field in fields do
        writeField writer field $"f{i}"
        i <- i + 1

    writer.Indent <- writer.Indent - 1

let writePrintFile filename modulename (productions:Production list) =
    use file = File.CreateText(filename)
    use writer = new IndentedTextWriter(file)
    writer.WriteLine $"module {modulename}"
    writer.WriteLine ()
    writer.WriteLine "open System.CodeDom.Compiler"
    writer.WriteLine ()
    writer.WriteLine "open Language"
    writer.WriteLine "open Tokens"
    writer.WriteLine "open Lexer"
    writer.WriteLine "open Parse"
    let mutable keyword = "let rec"

    for Production(name, cases, indent) in productions do
        writer.WriteLine ()
        writer.WriteLine $"{keyword} print{name} (writer:IndentedTextWriter) value ="
        keyword <- "and"
        writer.Indent <- writer.Indent + 1

        if indent then
            writer.WriteLine "let n = writer.Indent"
            writer.WriteLine "writer.Indent <- n + 1"
            writer.WriteLine ()

        writer.WriteLine "match value with"

        for UnionCase(name, fields) in cases do
            let n = List.length fields
            let fs = String.concat ", " (List.map (fun i -> $"f{i}") [0..n-1])
            writer.WriteLine $"| {name}({fs}) ->"
            writeCase writer fields

        if indent then
            writer.WriteLine ()
            writer.WriteLine "writer.Indent <- n"

        writer.Indent <- writer.Indent - 1
