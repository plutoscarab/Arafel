module Parse

open System
open System.Collections.Generic
open System.Text
open Lexer

let str text (c:TokenCursor) =
    if c.Str = text then
        [[], c.Next]
    else
        []

let token ctor (c:TokenCursor) =
    let eq =
        match c.Current with
        | Id x -> c.Current = ctor x
        | Nat x -> c.Current = ctor x
        | String x -> c.Current = ctor x
        | Operator x -> c.Current = ctor x
        | Superscript x -> c.Current = ctor x
        | _ -> false
    if eq then
        [c.Str, c.Next]
    else
        []
    
let rec exprStr expr =
    match expr with
    | Ebnf.StringLiteral s -> $"str \"{s}\""
    | Ebnf.Parens e -> "(" + (exprStr e) + ")"
    | Ebnf.NcName n ->
        if n = n.ToUpperInvariant() then
            "token " + n.Substring(0, 1) + n.Substring(1).ToLowerInvariant()
        else
            n.Substring(0, 1).ToLowerInvariant() + n.Substring(1) + "()"
    | Ebnf.Sequence items ->
        items |> List.map exprStr |> String.concat " &. "
    | Ebnf.Choice [(e, None)] ->
        exprStr e
    | Ebnf.Choice items ->
        items |> List.map (fun (e, name) ->
            let n = match name with | None -> "" | Some x -> x
            $"{exprStr e} |> capture {n}") |> String.concat " |. "
    | Ebnf.Primary (m, e) ->
        match m with
        | Ebnf.ZeroOrOne -> "%(" + (exprStr e) + ")"
        | Ebnf.ZeroOrMore -> $"-.({exprStr e})"
        | Ebnf.OneOrMore -> $"+.({exprStr e})"
    | Ebnf.Fields fields ->
        let s = new StringBuilder()
        s.Append("parser {\r\n") |> ignore
        let mutable i = 0
        for field in fields do
            s.Append($"        let! p{i} = {exprStr field}\r\n") |> ignore
            i <- i + 1
        s.Append("        return (") |> ignore
        i <- 0
        for field in fields do
            if i > 0 then s.Append(", ") |> ignore
            s.Append($"p{i}") |> ignore
            i <- i + 1
        s.Append(")\r\n") |> ignore
        s.Append("    }") |> ignore
        s.ToString()

type ParserBuilder() =
    member _.Bind(p, f) =
        fun c ->
            [ for (r, c2) in p c do
                yield! (f r) c2 ]
    member _.Return(r) =
        fun c -> [r, c]
    member _.ReturnFrom(p) = 
        p
    member _.Zero() =
        fun c -> []
    member _.Combine(p1, p2) =
        fun c ->
            (p1 c) @ (p2 c)
    member _.Delay(f) =
        f()

let parser = new ParserBuilder()

let (<|>) p1 p2 = 
    fun c -> 
        let r1 = p1 c
        let r2 = p2 c
        r1 @ r2

let (<+>) p1 p2 = 
    fun c ->
        [ for (r1, c1) in p1 c do
            for (r2, c2) in p2 c1 do
                yield r1 @ r2, c2 ]

let map f p =
    parser {
        let! r = p
        return f r
    }

let rec star p = 
    parser {
        return! parser { 
            let! head = p
            let! tail = star p
            return head::tail 
        } 
        return []
    }

let plus p = 
    parser {
        let! head = p
        let! tail = star p
        return head::tail
    }

let opt p =
    parser {
        let! r = p
        return r
    }

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
type ParseAttribute(syntax:string) =
    inherit System.Attribute()
    member _.Syntax = syntax