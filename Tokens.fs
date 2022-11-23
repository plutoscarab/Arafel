module Tokens

open System.Diagnostics
open Cursor

[<DebuggerDisplay("{ToString()}")>]
type Token = 
    | Keyword of Cspan
    | Identifier of Cspan
    | Operator of Cspan
    | Punctuation of Cspan
    | Nat of Cspan
    | Superscript of Cspan
    | String of Cspan
    | Bool of Cspan
    | Comment of Cspan
    | Error of Cursor

    with

    member this.Cursor() =
        match this with
        | Keyword s -> fst s
        | Identifier s -> fst s
        | Operator s -> fst s
        | Punctuation s -> fst s
        | Nat s -> fst s
        | Superscript s -> fst s
        | String s -> fst s
        | Bool s -> fst s
        | Comment s -> fst s
        | Error c -> c
    
    override this.ToString() =
        let txt =
            match this with
            | Keyword s -> "Keyword " + spanned s
            | Identifier s -> "Identifier " + spanned s
            | Operator s -> "Operator " + spanned s
            | Punctuation s -> "Punctuation " + spanned s
            | Nat s -> "Nat " + spanned s
            | Superscript s -> "Superscript " + spanned s
            | String s -> "String " + spanned s
            | Bool s -> "Bool " + spanned s
            | Comment s -> "Comment " + spanned s
            | Error c -> "Error " + (c.Str)
        let cursor = this.Cursor()
        sprintf "%s Line %d Pos %d" txt (cursor.line) (cursor.pos)

let tokenStr (token: Token) = token.ToString()

let tokenText (token: Token) =
    match token with
    | Keyword s -> spanned s
    | Identifier s -> spanned s
    | Operator s -> spanned s
    | Punctuation s -> spanned s
    | Nat s -> spanned s
    | Superscript s -> spanned s
    | String s -> spanned s
    | Bool s -> spanned s
    | Comment s -> spanned s
    | Error c -> c.Str

let tokenCursor (token: Token) = token.Cursor()

let tokenIndex =
    function
    | [] -> -1
    | t::_ -> (tokenCursor t).index