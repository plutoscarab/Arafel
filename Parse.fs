module Parse

open Lexer

type ParseTree = 
    | Empty
    | Error
    | Token of Token
    | Node of ParseTree list
    | Production of string * ParseTree

type Result = NoMatch | Ok of ParseTree * TokenCursor

let parseTreeFromList list =
    let rec flatten accum list =
        match list with
        | [] -> accum
        | h::t -> 
            match h with
            | Node sublist -> flatten (accum @ sublist) t
            | _ -> flatten (accum @ [h]) t
    let trimmed = list |> flatten [] |> List.filter (fun tree -> tree <> Empty)
    match trimmed with
    | [] -> Empty
    | [t] -> t
    | _ -> Node trimmed

let isNat (q:TokenCursor) =
    match q.Current with
    | Nat _ -> (Token q.Current, q.Next)
    | _ -> (Error, q)

let isString (q:TokenCursor) =
    match q.Current with
    | String _ -> (Token q.Current, q.Next)
    | _ -> (Error, q)

let isId (q:TokenCursor) =
    match q.Current with
    | Id _ -> (Token q.Current, q.Next)
    | _ -> (Error, q)

let isOperator (q:TokenCursor) =
    match q.Current with
    | Operator _ -> (Token q.Current, q.Next)
    | _ -> (Error, q)

let isText (s:string) (q:TokenCursor) =
    if tokenText (q.Current) = s then 
        (Token q.Current, q.Next) 
    else
        (Error, q)

let isSuperscript (q:TokenCursor) =
    match q.Current with
    | Superscript _ -> (Token q.Current, q.Next)
    | _ -> (Error, q)


let rec c choices (t:TokenCursor) =
    match choices with
    | [] -> (Error, t)
    | p::rest ->
        match p t with
        | (Error, _) -> c rest t
        | (result, next) -> (result, next)

let s items (t:TokenCursor) =
    let rec s' items list (t:TokenCursor) =
        match items with
        | [] -> (Node (List.rev list), t)
        | p::rest ->
            match p t with
            | (Error, _) -> (Error, t)
            | (result, next) -> s' rest (result::list) next
    s' items [] t

let t (a:obj) (t:TokenCursor) =
    match a with
    | :? (Lexer.Span -> Token) as f ->
        match t.Current with
        | Id x ->
            if t.Current = f x then (Token t.Current, t.Next) else (Error, t)
        | String x ->
            if t.Current = f x then (Token t.Current, t.Next) else (Error, t)
        | Operator x ->
            if t.Current = f x then (Token t.Current, t.Next) else (Error, t)
        | Nat x ->
            if t.Current = f x then (Token t.Current, t.Next) else (Error, t)
        | Superscript x ->
            if t.Current = f x then (Token t.Current, t.Next) else (Error, t)
        | _ -> (Error, t)
    | _ -> (Error, t)

let l str (t:TokenCursor) =
    if Lexer.tokenText t.Current = str then
        (Token t.Current, t.Next)
    else
        (Error, t)

let z parser (t:TokenCursor) =

    let rec z' list (t:TokenCursor) =
        match parser t with
        | (Error, _) -> (Node (List.rev list), t)
        | (result, next) -> z' (result::list) next

    z' [] t

let m parser (t:TokenCursor) =
    match z parser t with
    | (Error, _) -> (Error, t)
    | (Node [], _) -> (Error, t)
    | (result, next) -> (result, next)

let o parser (t:TokenCursor) =
    match parser t with
    | (Error, _) -> (Empty, t)
    | (result, next) -> (result, next)


let rec exprStr expr =
    match expr with
    | Ebnf.Choice [(expr, _)] ->
        exprStr expr
    | Ebnf.Choice list ->
        "c [" + (String.concat "; " (List.map (fun (e, name) ->
            let cmnt = 
                match name with
                | None -> ""
                | Some n -> "(* " + n + " *) "
            cmnt + (exprStr e)) list)) + "]"
    | Ebnf.Fields list ->
        "s [" + (String.concat "; " (List.map exprStr list)) + "]"
    | Ebnf.Sequence list ->
        "s [" + (String.concat "; " (List.map exprStr list)) + "]"
    | Ebnf.Primary (mult, expr) ->
        match mult with
        | Ebnf.ZeroOrOne -> "o (" + (exprStr expr) + ")"
        | Ebnf.ZeroOrMore -> "z (" + (exprStr expr) + ")"
        | Ebnf.OneOrMore -> "m (" + (exprStr expr) + ")"
    | Ebnf.Parens expr -> 
        exprStr expr
    | Ebnf.StringLiteral s ->
        "l \"" + s + "\""
    | Ebnf.NcName n ->
        if n = n.ToUpperInvariant() then
            "t " + n.Substring(0, 1) + n.Substring(1).ToLowerInvariant()
        else
            n.Substring(0, 1).ToLowerInvariant() + n.Substring(1)

type Parser<'r> = 
    | P of (TokenCursor -> 'r option * TokenCursor)
    static member (<|>)(P p1, P p2) =
        P (fun c ->
            let (m1, c1) = p1 c
            match m1 with
            | Some r1 -> (Some r1, c1)
            | None -> 
                let (m2, c2) = p2 c
                match m2 with
                | Some r2 -> (Some r2, c2)
                | None -> (None, c)
        )

type ParserBuilder() =
    member b.Zero() = P (fun c -> (None, c))
    member b.Return(r) = P (fun c -> (Some r, c))
    member b.ReturnFrom(p) = p
    member b.Delay(f) = f()

    member b.Bind(P p, f) = 
        P (fun c ->
            let (r, c') = p c
            match r with
            | None -> (None, c)
            | Some r' ->
                let (P p') = f r'
                p' c')

let parser = new ParserBuilder()