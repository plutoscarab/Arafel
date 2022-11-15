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
    | TokenP(s) ->
        writer.WriteLine $"writer.Write {name}"
    | LiteralP(s) ->
        let unboxed = s.Replace("□", " ").Replace("◁", "\\r\\n    ")
        writer.WriteLine $"writer.Write \"{unboxed}\""
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

    for Production(name, cases) in productions do
        writer.WriteLine ()
        writer.WriteLine $"{keyword} print{name} (writer:IndentedTextWriter) ="
        keyword <- "and"
        writer.Indent <- writer.Indent + 1
        writer.WriteLine "function"

        for UnionCase(name, fields) in cases do
            let n = List.length fields
            let fs = String.concat ", " (List.map (fun i -> $"f{i}") [0..n-1])
            writer.WriteLine $"| {name}({fs}) ->"
            writeCase writer fields

        writer.Indent <- writer.Indent - 1
