module Core

open Lexer

type ParseTree = Token of Token | Node of ParseTree list | Empty

type Result = NoMatch | Ok of ParseTree * TokenCursor

let rec c choices (t:TokenCursor) =
    match choices with
    | [] -> NoMatch
    | p::rest ->
        match p t with
        | NoMatch -> c rest t
        | Ok (result, next) -> Ok (result, next)

let s items (t:TokenCursor) =
    let rec s' items list (t:TokenCursor) =
        match items with
        | [] -> Ok (Node (List.rev list), t)
        | p::rest ->
            match p t with
            | NoMatch -> NoMatch
            | Ok (result, next) -> s' rest (result::list) next
    s' items [] t

let t (a:Span->Token) (t:TokenCursor) = NoMatch

let l str (t:TokenCursor) =
    if str = t.Str then
        Ok (Token t.Current, t.Next)
    else
        NoMatch

let z parser (t:TokenCursor) =

    let rec z' list (t:TokenCursor) =
        match parser t with
        | NoMatch -> Ok (Node (List.rev list), t)
        | Ok (result, next) -> z' (result::list) next

    z' [] t

let m parser (t:TokenCursor) =
    match z parser t with
    | NoMatch -> NoMatch
    | Ok (Node [], _) -> NoMatch
    | Ok (result, next) -> Ok (result, next)

let o parser (t:TokenCursor) =
    match parser t with
    | NoMatch -> Ok (Empty, t)
    | Ok (result, next) -> Ok (result, next)

let rec atom q =
    c [t Id; t Nat; t String; s [l "`"; t Operator; l "`"]; s [l "("; expr; l ")"]; lambda; s [l "let"; typedVar; l "="; expr; expr]; case] q

and case q =
    s [l "case"; expr; l "of"; m (s [pattern; c [l "→"; l "->"]; expr])] q

and expr q =
    s [atom; o (s [l "("; expr; z (s [l ","; expr]); l ")"])] q

and lambda q =
    s [l "("; typedVar; z (s [l ","; typedVar]); l ")"; l "="; expr] q

and monoType q =
    s [productType; z (s [c [l "→"; l "->"]; productType])] q

and pattern q =
    c [t Nat; t String; s [t Id; o (s [l "("; t Id; z (s [l ","; t Id]); l ")"])]] q

and productType q =
    s [typeAtom; z (s [c [l "×"; l "*"]; typeAtom])] q

and typ q =
    s [o (s [c [l "∀"; l "forall"]; m (t Id); l "."]); monoType] q

and typeAtom q =
    c [s [t Id; z (monoType)]; s [l "("; monoType; l ")"]] q

and typedVar q =
    s [t Id; l ":"; typ] q

