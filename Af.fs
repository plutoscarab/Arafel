open System
open System.CodeDom.Compiler
open System.Globalization
open System.IO
open System.Linq
open System.Numerics
open System.Text

open Reflect
open Parse
open Print
open Ebnf
open Syntax
open Typing
open Localisation

let rec orLast =
    function
    | [] -> []
    | [s] -> [s]
    | [p; u] -> [p; "or " + u]
    | h::t -> h::(orLast t)

let errStr e t =
    let ex = String.concat ", " (orLast e)

    match t with
    | [] ->
        loc.EndOfText(ex)
    | he::_ ->
        let cu = Tokens.tokenCursor he
        let tx = Tokens.tokenText he
        loc.LinePosToken(cu.line, cu.pos, tx, ex)
        
let sanityCheck =

    let src = File.ReadAllText("sample.af", Encoding.UTF8)
    let cursor = Cursor.makeCursor src
    let tokens = Lexer.tokenise Arafel.keywords cursor |> Seq.toList
    use file = File.CreateText("generated/pretty.af")
    use writer = new IndentedTextWriter(file)
    writer.Indent <- 1

    let mutable t = tokens

    while t <> [] do

        let (r, t2) = Arafel.expr t

        let err e t =
            raise (Exception (errStr e t))

        match r with
        | Nomatch e -> err e t
        | SyntaxError e -> err e t2
        | Match e ->
            writer.WriteLine ()
            Pretty.printExpr writer e
            writer.WriteLine ()

        t <- t2

let lines() =
    Seq.initInfinite (fun _ ->
        Console.Write "\naf] "
        Console.ReadLine().Trim())
    |> Seq.takeWhile (fun line -> line <> "quit")

let tokenise (src: string) =
    src
    |> Cursor.makeCursor
    |> Lexer.tokenise Arafel.keywords
    |> Seq.toList

type Value =
    | NatV of bigint
    | StringV of string
    | FunctionV of TypeExpr list list
    | ErrorV of string
    | NotImplV
    with
    override this.ToString() =
        match this with
        | NatV n -> n.ToString()
        | StringV s -> "\"" + s + "\""
        | FunctionV tss ->
            "[ " + (String.concat "; " 
                (List.map (fun ts ->
                    (String.concat " -> " (List.map (fun t ->
                        t.ToString()) ts))) tss)) + " ]"
        | ErrorV s -> s
        | NotImplV -> "Not implemented"

type Context = { 
    bound : Map<LexprName, Value>;
    ops : Set<Operation>;
    }

type Resolution =
    | ContextR of Context
    | ValueR of Value
    | ErrorR of string
    | NotImplR

let rec evalId context id =
    match context.bound.TryFind id with
    | None -> ErrorV (loc.IsNotBound(id))
    | Some v -> v

and evalCases context cases =
    NotImplV

and evalIfThen context ifThen =
    NotImplV

and evalOp context prelude opname args =

    let matches =
        context.ops 
        |> Set.filter (fun (Operation (name, _)) -> name = opname)
        |> Set.map (fun (Operation (_, args)) -> args)

    if Set.count(matches) = 0 then
        ErrorV (loc.OperatorNotBound(opname))
    else
        let argc = List.length(args)
        let cmax = (matches |> Set.map (List.length) |> Seq.max) - 1

        if argc = 0 then
            FunctionV (Set.toList matches)
        elif argc > cmax then
            ErrorV (loc.TooManyArguments(argc, opname, cmax))
        else
            NotImplV

and evalExpr context (Expr(prelude, atom, args, postfix)) =
    let value =
        match atom with
        | NatA n -> NatV n
        | StringA s -> StringV s
        | OperatorA s -> evalOp context prelude s args
        | LambdaA e -> NotImplV
        | ParensA e -> evalExpr context e
        | IdentifierA id ->
            let r = Rune.GetRuneAt(id, 0)
            if Rune.IsLetter(r) || r.ToString() = "_" then
                evalId context (IdentifierN id)
            else
                evalId context (OperatorN id)
        | CasesA e -> evalCases context e
        | IfThenA e -> evalIfThen context e
    value

let applyLet context lexpr expr =
    match lexpr with
    | Lexpr (id, []) ->
        match evalId context id with
        | ErrorV _ ->
            match evalExpr context expr with
            | ErrorV e -> ErrorR e
            | NotImplV -> NotImplR
            | v -> ContextR { context with bound = Map.add id v (context.bound) }
        | _ -> ErrorR (loc.IsAlreadyBound(id))
    | _ -> NotImplR

let apply context =
    function
    | LetCmd (LetDecl(lexpr, expr)) ->
        applyLet context lexpr expr
    | TypeCmd (TypeDecl(id, parms, polytype)) ->
        NotImplR
    | ExprCmd e ->
        match evalExpr context e with
        | ErrorV e -> ErrorR e
        | NotImplV -> NotImplR
        | v -> ValueR v

let execute ctx src =
    let mutable context = ctx
    let t = tokenise src
    let mutable t1 = t

    while t1 <> [] do
        let (m, t2) = Arafel.command t1

        let resolution =
            match m with
            | Nomatch e ->
                ErrorR (errStr e t1)
            | SyntaxError e ->
                ErrorR (errStr e t2)
            | Match code ->
                use writer = new IndentedTextWriter(Console.Out)
                Pretty.printCommand writer code
                writer.WriteLine ()
                apply context code

        match resolution with
        | ContextR c -> context <- c
        | ValueR v -> Console.WriteLine (v.ToString())
        | ErrorR e -> Console.WriteLine e
        | NotImplR -> Console.WriteLine (loc.CapabilityNotYet())

        if t1 = t2 then
            t1 <- []
        else
            t1 <- t2

    context

let main =
    Console.OutputEncoding <- Encoding.UTF8
    sanityCheck
    Console.WriteLine (loc.EnterQuitTo())
    let mutable src = ""

    let mutable context = {
        bound = Map.empty<LexprName, Value>;
        ops = nativeOps;
    }

    for line in lines() do
        let d = line.EndsWith("\\")
        let c = if d then line.Substring(0, line.Length - 1).Trim() else line
        src <- src + " " + c

        if not d then
            context <- execute context src
            src <- ""
    0