module Parse

open System
open System.CodeDom.Compiler
open System.Collections.Generic
open System.IO
open System.Text

open Arafel.CodeDom
open Cursor
open Lexer
open Tokens

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
type ParseAttribute(syntax:string) =
    inherit System.Attribute()
    member _.Syntax = syntax

type Result<'r> =
    | Match of 'r
    | Nomatch of string list
    | SyntaxError of string list

let mergeErrors es =
    List.ofSeq (Seq.sort (Seq.distinct (Seq.ofList es)))

type ParserBuilder() =
    member _.Bind(p, f) =
        fun t ->
            let (m, t2) = p t
            match m with
            | Nomatch e -> (Nomatch e), t
            | SyntaxError e -> (SyntaxError e), t2
            | Match r -> (f r) t2
    member _.Combine(p1, p2) =
        fun t ->
            let (m, t2) = p1 t
            match m with
            | Match r -> (Match r), t2
            | SyntaxError e -> (SyntaxError e), t2
            | Nomatch e1 ->
                let (m, t2) = p2 t
                match m with
                | Match r -> (Match r), t2
                | SyntaxError e -> (SyntaxError e), t2
                | Nomatch e2 -> (Nomatch (mergeErrors (e1 @ e2))), t
    member _.Delay(f) =
        f()
    member _.Return(r) =
        fun t -> (Match r), t
    member _.ReturnFrom(p) = 
        p
    member _.Zero() =
        fun t -> Nomatch [], t

let parser = new ParserBuilder()

let stringToken (ctor:Cspan -> Token) ctorName =
    fun t ->
        let nm = (Nomatch [$"{ctorName} token"]), t
        match t with
        | [] -> nm
        | first::rest ->
            let f x =
                if first = ctor x
                    then (Match (tokenText first)), rest
                    else nm
            match first with
            | Identifier x  -> f x
            | String x      -> f x
            | Bool x        -> f x
            | Operator x    -> f x
            | Keyword x     -> f x
            | Nat x         -> f x
            | Superscript x -> f x
            | Comment x     -> f x
            | Punctuation x -> f x
            | Error c       -> nm

let private ten =
    bigint 10

let private parseNat (s:string) =

    let rec parseNat' s n =
        match s with
        | [] -> true, n
        | r::rest ->
            let rs = r.ToString()
            if (rs = "_") then
                parseNat' rest n
            else
                let v = Convert.ToInt32(Rune.GetNumericValue r)
                let u = if v = -1 then (superchars.IndexOf(rs)) else v
                if u = -1 then
                    false, bigint 0
                else 
                    parseNat' rest (n * ten + bigint u)

    parseNat' (s.EnumerateRunes() |> Seq.toList) (bigint 0)

let private parseBool =
    function
    | "false" | "𝗙" -> (true, false)
    | "true" | "𝗧" -> (true, true)
    | _ -> (false, false)
    
let bigintToken (ctor:Cspan -> Token) ctorName =
    fun t ->
        let (m, t2) = (stringToken ctor ctorName) t
        match m with
        | Nomatch e -> (Nomatch e), t
        | SyntaxError e -> (SyntaxError e), t2
        | Match s ->
            match parseNat s with
            | (false, _) -> (Nomatch ["Nat value"]), t
            | (true, n) -> (Match n), t2

let boolToken (ctor:Cspan -> Token) ctorName =
    fun t ->
        let (m, t2) = (stringToken ctor ctorName) t
        match m with
        | Nomatch e -> (Nomatch e), t
        | SyntaxError e -> (SyntaxError e), t2
        | Match s ->
            match parseBool s with
            | (false, _) -> (Nomatch ["Bool value"]), t
            | (true, b) -> (Match b), t2

let literal (s: string) =
    fun t ->
        match t with
        | [] -> Nomatch [$"«{s}»"], t
        | first::rest ->
            if tokenText first = s
                then (Match ()), rest
                else Nomatch [$"«{s}»"], t

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

let checkpoint p =
    fun t ->
        let (m, t2) = p t
        match m with
        | Nomatch e -> (SyntaxError e), t2
        | SyntaxError e -> (SyntaxError e), t2
        | Match r -> (Match r), t2

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
        | Nomatch _ -> (Match []), t
        | SyntaxError e -> (SyntaxError e), t2
        | Match r ->
            let (m2, t3) = (zeroOrMore p) t2
            match m2 with
            | Nomatch _ -> (Match [r]), t2
            | SyntaxError e -> (SyntaxError e), t3
            | Match rs -> (Match (r::rs)), t3

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

let rec getKeywords =
    function
    | ProductionP (_, _) -> Seq.empty
    | TokenP _ -> Seq.empty
    | LiteralP s ->
        let u = plain s
        if Rune.IsLetter(Rune.GetRuneAt(u, 0)) then seq { u } else Seq.empty
    | OutP (_, p) -> getKeywords p
    | OptionP p -> getKeywords p
    | OptionListP p -> getKeywords p
    | ListP p -> getKeywords p
    | NonEmptyListP p -> getKeywords p
    | CheckpointP p -> getKeywords p
    | AndP (p, q) ->
        Seq.concat [getKeywords p; getKeywords q]
    | OrP (p, q) ->
        Seq.concat [getKeywords p; getKeywords q]
    | DelimitedP (p, q) ->
        Seq.concat [getKeywords p; getKeywords q]
    | SurroundP (p, q, r) ->
        Seq.concat [getKeywords p; getKeywords q; getKeywords r]

let private formatter =
    function
    | '_' -> Raw
    | '␤' -> Newline
    | '␏' -> Indent
    | '␎' -> Outdent
    | '␠' -> Space
    | _ -> raise (Exception())

let rec private decodeParser (ps: string list) =
    match ps with
    | p::rest when p.Contains('_') ->
        ProductionP (formatter (p[0]), formatter (p[^0])), rest
    | "⚠"::rest ->
        let (p, rest') = decodeParser rest
        CheckpointP p, rest'
    | "opt"::rest ->
        let (p, rest') = decodeParser rest
        OptionP p, rest'
    | "opt[]"::rest ->
        let (p, rest') = decodeParser rest
        OptionListP p, rest'
    | "0+"::rest ->
        let (p, rest') = decodeParser rest
        ListP p, rest'
    | "1+"::rest ->
        let (p, rest') = decodeParser rest
        NonEmptyListP p, rest'
    | "and"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        AndP (p, q), rest''
    | "or"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        OrP (p, q), rest''
    | "delim"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        DelimitedP (p, q), rest''
    | "surr"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        let (r, rest''') = decodeParser rest''
        SurroundP (p, q, r), rest'''
    | "out"::s::rest when s.Length > 2 && s.StartsWith("'") && s.EndsWith("'") ->
        let (p, rest') = decodeParser rest
        OutP (s.Substring(1, s.Length - 2), p), rest'
    | s::rest when s.Length > 2 && s.StartsWith("'") && s.EndsWith("'") ->
        LiteralP (s.Substring(1, s.Length - 2)), rest
    | s::rest when s = s.ToUpperInvariant() && s <> s.ToLowerInvariant() ->
        TokenP (s), rest
    | _ ->
        raise (NotImplementedException())

let decodeParserStr (s:string) =
    let r, n = decodeParser (s.Split(' ') |> List.ofArray)
    if n <> [] then raise (Exception "")
    r

let rec private writeParser (writer:IndentedTextWriter) parser primaryType =
    match parser with

    | ProductionP (_, _) ->

        match primaryType with
        | ProductionType name -> writer.Write $"parse{name}"
        | _ -> raise (NotImplementedException())

    | TokenP s ->

        let tokenType =
            match primaryType with
            | StringType -> "stringToken"
            | BigintType -> "bigintToken"
            | BoolType -> "boolToken"
            | _ -> raise (NotImplementedException())

        let u = plain s
        let tokenCtor = u.Substring(0, 1) + u.Substring(1).ToLowerInvariant()
        writer.Write $"{tokenType} {tokenCtor} \"{tokenCtor}\""

    | LiteralP s ->

        writer.Write $"literal \"{plain s}\""

    | OutP (_, p) ->

        writeParser writer p primaryType

    | CheckpointP p ->

        writer.Write "checkpoint ("
        writeParser writer p primaryType
        writer.Write ")"

    | OptionP p ->

        writer.Write "option ("
        writeParser writer p primaryType
        writer.Write ")"

    | OptionListP p ->

        writer.Write "optionlist ("
        writeParser writer p primaryType
        writer.Write ")"

    | ListP p ->

        writer.Write "zeroOrMore ("
        writeParser writer p primaryType
        writer.Write ")"

    | NonEmptyListP p ->

        writer.Write "oneOrMore ("
        writeParser writer p primaryType
        writer.Write ")"

    | AndP (p, q) ->

        writer.Write "andThen ("
        writeParser writer p primaryType
        writer.Write ") ("
        writeParser writer q primaryType
        writer.Write ")"

    | OrP (p, q) ->

        writer.Write "orElse ("
        writeParser writer p primaryType
        writer.Write ") ("
        writeParser writer q primaryType
        writer.Write ")"

    | DelimitedP (d, p) ->

        writer.Write "delimited ("
        writeParser writer d primaryType
        writer.Write ") ("
        writeParser writer p primaryType
        writer.Write ")"

    | SurroundP (a, b, p) ->

        writer.Write "surround ("
        writeParser writer a primaryType
        writer.Write ") ("
        writeParser writer b primaryType
        writer.Write ") ("
        writeParser writer p primaryType
        writer.Write ")"

let private writeField (writer: IndentedTextWriter) pname (TupleField(fname, primaryType, _, parser)) =
    writeParser writer parser primaryType

let private writeCase (writer: IndentedTextWriter) pname (UnionCase(name, fields)) asRecursive =
    let n = writer.Indent
    //writer.WriteLine "parser {"
    writer.WriteLine "fun t0 ->"
    writer.Indent <- writer.Indent + 1
    let toSkip = if asRecursive then 1 else 0
    let mutable fnames = if asRecursive then [$"base{pname}"] else []
    let mutable i = 0

    for field in List.skip toSkip fields do
        let (TupleField(fname, _, _, _)) = field
        fnames <- fname::fnames
        writer.Write $"let (r{i + 1}, t{i + 1}) = ("
        writeField writer pname field
        writer.WriteLine $") t{i}"
        writer.WriteLine $"match r{i + 1} with"
        writer.WriteLine $"| Match {fname} ->"
        writer.Indent <- writer.Indent + 1
        i <- i + 1

    let fs = fnames |> List.rev |> String.concat ", "
    writer.WriteLine $"Match ({name}({fs})), t{i}"

    while i > 0 do
        writer.Indent <- writer.Indent - 1
        writer.WriteLine $"| SyntaxError e -> SyntaxError e, t{i}"
        writer.WriteLine $"| Nomatch e -> Nomatch e, t{i - 1}"
        i <- i - 1

    //writer.Indent <- writer.Indent - 1
    //writer.WriteLine "}"
    writer.Indent <- n

let isRecursiveCase pname (UnionCase(_, fields)) =
    let (TupleField(_, primaryType, _, p)) = fields.[0]
    match primaryType, p with
    | ProductionType pt, ProductionP (_, _) -> pt = pname
    | _ -> false

let rec repeat baseValue suffix =
    fun t ->
        match (suffix baseValue) t with
        | Nomatch e, _ -> Match baseValue, t
        | SyntaxError e, t2 -> SyntaxError e, t2
        | Match r, t2 -> (repeat r suffix) t2

let writeParserFile filename modulename (productions:Production list) (keywords:string seq) =
    use file = File.CreateText(filename)
    use writer = new IndentedTextWriter(file)
    writer.WriteLine $"module {modulename}"
    writer.WriteLine "// Generated code. Do not edit."
    writer.WriteLine ()
    writer.WriteLine "open Tokens"
    writer.WriteLine "open Lexer"
    writer.WriteLine "open Parse"
    writer.WriteLine "open Syntax"
    writer.WriteLine ()
    writer.WriteLine $"let keywords = Set ["
    writer.Write "    "
    let mutable col = 4

    for kw in keywords do
        if col + kw.Length > 77 then
            writer.Write "\r\n    "
            col <- 4
        writer.Write $"\"{kw}\"; "
        col <- col + kw.Length + 4

    writer.WriteLine ()
    writer.WriteLine $"]"

    let mutable keyword = "let rec"

    for Production(pname, cases, indent) in productions do
        writer.WriteLine ()
        writer.WriteLine $"{keyword} parse{pname}' () ="
        keyword <- "and"
        writer.Indent <- writer.Indent + 1

        if List.length(cases) = 1 then
            writeCase writer pname cases.[0] false
        else
            let (recursiveCases, normalCases) =
                List.partition (isRecursiveCase pname) cases

            let writeCases cs =
                writer.WriteLine "parser {"
                writer.Indent <- writer.Indent + 1

                for case in cs do
                    writer.Write "return! "
                    writeCase writer pname case false

                writer.Indent <- writer.Indent - 1
                writer.WriteLine "}"

            if List.isEmpty recursiveCases then
                writeCases cases
            else
                writer.Write "let baseParser = "
                writeCases normalCases

                writer.WriteLine $"let suffixes base{pname} = parser {{"
                writer.Indent <- writer.Indent + 1
                
                for case in recursiveCases do
                    let (UnionCase(uname, _)) = case
                    writer.Write "return! "
                    writeCase writer pname case true

                writer.Indent <- writer.Indent - 1
                writer.WriteLine "}"
                writer.WriteLine "parser {"
                writer.Indent <- writer.Indent + 1
                writer.WriteLine $"let! base{pname} = baseParser"
                writer.WriteLine $"return! repeat base{pname} suffixes"
                writer.Indent <- writer.Indent - 1
                writer.WriteLine "}"

        writer.Indent <- writer.Indent - 1

    writer.WriteLine ()

    for Production(pname, cases, indent) in productions do
        writer.WriteLine $"and parse{pname} = parse{pname}'()"
