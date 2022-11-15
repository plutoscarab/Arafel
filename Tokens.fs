module Tokens

open Cursor

type Token = 
    | Keyword of Cspan
    | Id of Cspan
    | Operator of Cspan
    | Punctuation of Cspan
    | Nat of Cspan
    | Superscript of Cspan
    | String of Cspan
    | Comment of Cspan
    | Error of Cursor
    | EndOfText

let tokenStr (token:Token) =
    match token with
    | Keyword s -> "Keyword " + spanned s
    | Id s -> "Id " + spanned s
    | Operator s -> "Operator " + spanned s
    | Punctuation s -> "Punctuation " + spanned s
    | Nat s -> "Nat " + spanned s
    | Superscript s -> "Superscript " + spanned s
    | String s -> "String " + spanned s
    | Comment s -> "Comment " + spanned s
    | Error c -> "Error " + (c.Str)
    | EndOfText -> ""

let tokenText (token:Token) =
    match token with
    | Keyword s -> spanned s
    | Id s -> spanned s
    | Operator s -> spanned s
    | Punctuation s -> spanned s
    | Nat s -> spanned s
    | Superscript s -> spanned s
    | String s -> spanned s
    | Comment s -> spanned s
    | Error c -> c.Str
    | EndOfText -> ""

type TokenCursor =
    { source: Token array;
      index: int }

    member c.More = c.index < c.source.Length

    member c.Current = if c.More then c.source[c.index] else EndOfText

    member c.Str = tokenStr (c.Current)

    member c.Next = 
        if c.More then 
            { c with index = c.index + 1 }
        else 
            c
