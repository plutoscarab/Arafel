module Parse

open System
open System.CodeDom.Compiler
open System.Collections.Generic
open System.IO
open System.Text

open Cursor
open Lexer
open Tokens

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
type ParseAttribute(syntax:string) =
    inherit System.Attribute()
    member _.Syntax = syntax

type ParserBuilder() =
    member _.Bind(p, f) =
        fun t ->
            let (m, t2) = p t
            match m with
            | None -> None, t
            | Some r -> (f r) t2
    member _.Combine(p1, p2) =
        fun t ->
            let (m, t2) = p1 t
            match m with
            | None -> p2 t
            | Some r -> (Some r), t2
    member _.Delay(f) =
        f()
    member _.Return(r) =
        fun t -> (Some r), t
    member _.ReturnFrom(p) = 
        p
    member _.Zero() =
        fun t -> None, t

let parser = new ParserBuilder()

let stringToken (ctor:Cspan -> Token) =
    fun t ->
        match t with
        | [] -> None, t
        | first::rest ->
            match first with
            | Id x          -> if first = ctor x then (Some (tokenText first)), rest else None, t
            | String x      -> if first = ctor x then (Some (tokenText first)), rest else None, t
            | Operator x    -> if first = ctor x then (Some (tokenText first)), rest else None, t
            | Keyword x     -> if first = ctor x then (Some (tokenText first)), rest else None, t
            | Nat x         -> if first = ctor x then (Some (tokenText first)), rest else None, t
            | Superscript x -> if first = ctor x then (Some (tokenText first)), rest else None, t
            | Comment x     -> if first = ctor x then (Some (tokenText first)), rest else None, t
            | Punctuation x -> if first = ctor x then (Some (tokenText first)), rest else None, t
            | Error c       -> None, t
            | EndOfText     -> None, t

let private ten =
    bigint 10

let private parseNat (s:string) =

    let rec parseNat' s n =
        match s with
        | [] -> n
        | r::rest ->
            let rs = r.ToString()
            if (rs = "_") then
                parseNat' rest n
            else
                let v = Convert.ToInt32(Rune.GetNumericValue r)
                let u = if v = -1 then ("⁰¹²³⁴⁵⁶⁷⁸⁹".IndexOf(rs)) else v
                parseNat' rest (n * ten + bigint u)

    parseNat' (s.EnumerateRunes() |> Seq.toList) (bigint 0)

let bigintToken (ctor:Cspan -> Token) =
    fun t ->
        let (m, t2) = (stringToken ctor) t
        match m with
        | None -> None, t
        | Some s -> (Some (parseNat s)), t2

let literal (s: string) =
    fun t ->
        match t with
        | [] -> None, t
        | first::rest ->
            if tokenText first = s.Replace("□", "").Replace("◁", "")
                then (Some ()), rest
                else None, t

let andThen p q =
    parser {
        let! _ = p
        let! r = q
        return r
    }

let orElse p q =
    parser {
        return! p
        return! q
    }

let option p =
    parser {
        return! parser {
            let! r = p
            return Some r
        }
        return None
    }

let optionlist p =
    parser {
        return! parser {
            let! r = p
            return r
        }
        return []
    }

let rec zeroOrMore p =
    fun t ->
        let (m, t2) = p t
        match m with
        | None -> (Some []), t
        | Some r ->
            let (m2, t3) = (zeroOrMore p) t2
            match m2 with
            | None -> (Some [r]), t2
            | Some rs -> (Some (r::rs)), t3

and oneOrMore p =
    parser {
        let! first = p
        let! rest = zeroOrMore p
        return first::rest
    }

let delimited d p =
    parser {
        let! first = p
        let! rest = zeroOrMore (andThen d p)
        return first::rest
    }

let surround a b p =
    parser {
        let! _ = a
        let! r = p
        let! _ = b
        return r
    }


type Parser =
    | ProductionP
    | ProductionLineP
    | ProductionIndentP
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

type PrimaryType =
    | StringType
    | BigintType
    | ProductionType of string

type Multiplicity =
    | SingleM
    | OptionM
    | ListM

type TupleField =
    | TupleField of PrimaryType * Multiplicity * Parser

type UnionCase =
    | UnionCase of string * TupleField list

type Production =
    | Production of string * UnionCase list * bool

let rec private writeParser (writer:IndentedTextWriter) parser primaryType =
    match parser with
    | ProductionP
    | ProductionLineP
    | ProductionIndentP ->
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

let private writeField (writer:IndentedTextWriter) (TupleField(primaryType, _, parser)) =
    writeParser writer parser primaryType
    writer.WriteLine ()

let private writeCase (writer:IndentedTextWriter) (UnionCase(name, fields)) =
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
    writer.WriteLine "open Tokens"
    writer.WriteLine "open Lexer"
    writer.WriteLine "open Parse"
    let mutable keyword = "let rec"

    for Production(name, cases, indent) in productions do
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
