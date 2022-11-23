module Print

open System
open System.CodeDom.Compiler
open System.Collections.Generic
open System.IO
open System.Text

open Arafel.CodeDom
open Cursor
open Lexer
open Tokens

type IndentAttribute() = inherit System.Attribute()

let superscriptMap = Map [
    '0', '⁰';
    '1', '¹';
    '2', '²';
    '3', '³';
    '4', '⁴';
    '5', '⁵';
    '6', '⁶';
    '7', '⁷';
    '8', '⁸';
    '9', '⁹';
]

let charToSuper ch =
    match superscriptMap.TryFind ch with
    | None -> ch
    | Some s -> s

let toSuperscript value =
    value.ToString() |> Seq.map charToSuper |> String.Concat

let writeSafe (writer: IndentedTextWriter) value =
    let s = value.ToString()
    if s.EndsWith("\r\n") then
        let s' = s.Substring(0, s.Length - 2)
        writer.WriteLine s'
    elif s.EndsWith("\n") || s.EndsWith("\r") then
        let s' = s.Substring(0, s.Length - 1)
        writer.WriteLine s'
    else
        writer.Write s

let private writeFormatter (writer:IndentedTextWriter) =
    function
    | Raw -> ignore()
    | Newline -> writer.WriteLine "writer.WriteLine ()"
    | Indent -> writer.WriteLine "writer.Indent <- writer.Indent + 1"
    | Outdent ->
        writer.WriteLine "writer.Indent <- writer.Indent - 1"
        writer.WriteLine ()
    | Space -> writer.WriteLine "writer.Write \" \""

let rec private writeParser (writer:IndentedTextWriter) parser primaryType name =
    match parser with
    | ProductionP(pre, post) ->

        writeFormatter writer pre

        match primaryType with
        | StringType
        | BigintType ->
            writer.WriteLine $"writer.Write {name}"
        | BoolType ->
            writer.WriteLine $"writer.Write {name}.ToString().ToLowerInvariant()"
        | ProductionType p ->
            writer.WriteLine $"print{p} writer {name}"

        writeFormatter writer post

        if pre = Indent then
            writer.WriteLine "writer.Indent <- writer.Indent - 1"

    | TokenP(t) ->
        let mutable s = t
        let ss = s.StartsWith("␑")

        while s.[0] = Char.ToLowerInvariant(s.[0]) do
            match s.[0] with
            | '␤' -> writer.WriteLine "writer.WriteLine ()"
            | '␅' -> writer.WriteLine "writer.Write \"\\\"\""
            | '␠' -> writer.WriteLine "writer.Write \" \""
            | _ -> ignore ()
            s <- s.Substring(1)

        if ss then
            writer.WriteLine $"let s = toSuperscript {name}"
            writer.WriteLine "writeSafe writer s"
        else
            match primaryType with
            | BoolType ->
                writer.WriteLine $"let b = {name}.ToString().ToLowerInvariant()"
                writer.WriteLine "writer.Write b"
            | _ ->
                writer.WriteLine $"writeSafe writer {name}"

        while s.Length > 0 && s.[0] <> Char.ToLowerInvariant(s.[0]) do
            s <- s.Substring(1)

        while s.Length > 0 do
            match s.[0] with
            | '␤' -> writer.WriteLine "writer.WriteLine ()"
            | '␅' -> writer.WriteLine "writer.Write \"\\\"\""
            | '␠' -> writer.WriteLine "writer.Write \" \""
            | _ -> ignore ()
            s <- s.Substring(1)

    | LiteralP(s) ->

        let u = s.Replace("␠", " ")
                 .Split('␤')

        for i in [0..u.Length-1] do
            let v = plain u.[i]
            if i < u.Length - 1 then
                writer.WriteLine $"writer.WriteLine \"{v}\""
            elif v <> "" then
                writer.WriteLine $"writer.Write \"{v}\""

        if s.EndsWith("␏") then
            writer.WriteLine "writer.Indent <- writer.Indent + 1"

    | OutP (s, p) ->

        let u = s.Replace("␠", " ")
                 .Split('␤')

        for i in [0..u.Length-1] do
            let v = plain u.[i]
            if i < u.Length - 1 then
                writer.WriteLine $"writer.WriteLine \"{v}\""
            elif v <> "" then
                writer.WriteLine $"writer.Write \"{v}\""

        writeParser writer p primaryType name

    | CheckpointP(p) ->

        writeParser writer p primaryType name

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

        writeParser writer q primaryType name

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

let fieldName =
    function
    | TupleField (name, _, _, _) -> name

let private writeField writer (TupleField(name, primaryType, _, parser)) =
    writeParser writer parser primaryType name

let private writeCase (writer:IndentedTextWriter) fields =
    writer.Indent <- writer.Indent + 1
    List.iter (writeField writer) fields
    writer.Indent <- writer.Indent - 1

let writePrintFile filename modulename (productions:Production list) =
    use file = File.CreateText(filename)
    use writer = new IndentedTextWriter(file)
    writer.WriteLine $"module {modulename}"
    writer.WriteLine "// Generated code. Do not edit."
    writer.WriteLine ()
    writer.WriteLine "open System.CodeDom.Compiler"
    writer.WriteLine ()
    writer.WriteLine "open Tokens"
    writer.WriteLine "open Lexer"
    writer.WriteLine "open Parse"
    writer.WriteLine "open Print"
    writer.WriteLine "open Syntax"

    let mutable keyword = "let rec"

    for Production(name, cases, indent) in productions do
        writer.WriteLine ()
        writer.WriteLine $"{keyword} print{name} (writer:IndentedTextWriter) value ="
        keyword <- "and"
        writer.Indent <- writer.Indent + 1

        writer.WriteLine "let n = writer.Indent"

        if indent then
            writer.WriteLine "writer.Indent <- n + 1"

        writer.WriteLine ()
        writer.WriteLine "match value with"

        for UnionCase(name, fields) in cases do
            let fs = String.concat ", " (List.map fieldName fields)
            writer.WriteLine $"| {name}({fs}) ->"
            writeCase writer fields

        writer.WriteLine ()
        writer.WriteLine "writer.Indent <- n"
        writer.Indent <- writer.Indent - 1
