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

let tokenCursor (token:Token) =
    match token with
    | Keyword s -> fst s
    | Id s -> fst s
    | Operator s -> fst s
    | Punctuation s -> fst s
    | Nat s -> fst s
    | Superscript s -> fst s
    | String s -> fst s
    | Comment s -> fst s
    | Error c -> c