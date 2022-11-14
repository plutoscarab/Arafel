module Parse

open System
open System.Collections.Generic
open System.Text

open Cursor
open Lexer

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
type ParseAttribute(syntax:string) =
    inherit System.Attribute()
    member _.Syntax = syntax

type Result<'r> = 'r option * Token list

type Parser<'r> = Token list -> Result<'r>

type ParserBuilder() =
    member _.Bind(p, f) =
        fun t ->
            let (m, t2) = p t
            match m with
            | None -> None, t
            | Some r -> (f r) t2
    member _.Return(r) =
        fun t -> (Some r), t
    member _.ReturnFrom(p) = 
        p
    member _.Zero() =
        fun t -> None, t
    member _.Combine(p1, p2) =
        fun t ->
            let (m, t2) = p1 t
            match m with
            | None -> p2 t
            | Some r -> (Some r), t2
    member _.Delay(f) =
        f()

let parser = new ParserBuilder()

let stringToken (ctor:Cspan -> Token) =
    fun t ->
        match t with
        | [] -> None, t
        | first::rest ->
            match first with
            | Id x when first = ctor x -> (Some (tokenText first)), rest
            | String x when first = ctor x -> (Some (tokenText first)), rest
            | Operator x when first = ctor x -> (Some (tokenText first)), rest
            | Keyword x when first = ctor x -> (Some (tokenText first)), rest
            | Nat x when first = ctor x -> (Some (tokenText first)), rest
            | Superscript x when first = ctor x -> (Some (tokenText first)), rest
            | _ -> None, t

let ten =
    bigint 10

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

let parseNat (s:string) =
    parseNat' (s.EnumerateRunes() |> Seq.toList) (bigint 0)

let bigintToken (ctor:Cspan -> Token) : Parser<bigint> =
    fun t ->
        let (m, t2) = (stringToken ctor) t
        match m with
        | None -> None, t
        | Some s -> (Some (parseNat s)), t2

let literal s =
    fun t ->
        match t with
        | [] -> None, t
        | first::rest ->
            if tokenText first = s
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
    andThen p (zeroOrMore (andThen d p))

let surround a b p =
    parser {
        let! _ = a
        let! r = p
        let! _ = b
        return r
    }
