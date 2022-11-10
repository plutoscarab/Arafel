open System
open System.CodeDom.Compiler
open System.Globalization
open System.IO
open System.Linq
open System.Numerics
open System.Text

let rec build (writer:IndentedTextWriter) expr depth param =
    let indent =
        match expr with
        | Ebnf.Parens _ -> 0
        | _ -> 1
    let prefix = char (depth + int 'a')
    let deeper = depth + 1
    let w:(string->unit) = writer.WriteLine
    let v:(string->unit) = writer.Write
    match expr with
    | Ebnf.Choice list ->
        writer.WriteLine $"// {Ebnf.show expr}"
        writer.Indent <- writer.Indent + indent
        for (item, i) in List.zip list [1..List.length(list)] do
            v $"let ({prefix}t{i}, {prefix}c{i}) = "
            build writer item deeper param
            w $"if {prefix}t{i} <> Error then ({prefix}t{i}, {prefix}c{i}) else"
        let items = [1..List.length(list)] |> List.map (fun i -> $"{prefix}t{i}") |> String.concat "; "
        w $"(Error, {param})"
        writer.Indent <- writer.Indent - indent
    | Ebnf.Sequence list ->
        writer.WriteLine $"// {Ebnf.show expr}"
        writer.Indent <- writer.Indent + indent
        for (item, i) in List.zip list [1..List.length(list)] do
            v $"let ({prefix}t{i}, {prefix}c{i}) = "
            if i = 1 then
                build writer item deeper param
            else
                build writer item deeper $"{prefix}c{i-1}"
            w $"if {prefix}t{i} = Error then (Error, {param}) else"
        let items = [1..List.length(list)] |> List.map (fun i -> $"{prefix}t{i}") |> String.concat "; "
        w $"(parseTreeFromList [{items}], {prefix}c{List.length(list)})"
        writer.Indent <- writer.Indent - indent
    | Ebnf.Primary (mult, subexpr) ->
        writer.WriteLine $"// {Ebnf.show expr}"
        writer.Indent <- writer.Indent + indent
        match mult with
        | Ebnf.ZeroOrOne ->
            v $"let ({prefix}t, {prefix}c) = "
            build writer subexpr deeper param
            w $"match {prefix}t with"
            w $"| Error -> (Empty, {param})"
            w $"| _ -> ({prefix}t, {prefix}c)"
        | Ebnf.ZeroOrMore ->
            w $"let rec z list ({prefix}q:TokenCursor) ="
            writer.Indent <- writer.Indent + 1
            v $"let ({prefix}t, {prefix}c) = "
            build writer subexpr deeper $"{prefix}q"
            w $"match {prefix}t with"
            w $"| Error -> (parseTreeFromList (List.rev list), {prefix}q)"
            w $"| _ -> z ({prefix}t :: list) {prefix}c"
            writer.Indent <- writer.Indent - 1
            w $"z [] {param}"
        | Ebnf.OneOrMore ->
            w $"let rec z list ({prefix}q:TokenCursor) ="
            writer.Indent <- writer.Indent + 1
            v $"let ({prefix}t, {prefix}c) = "
            build writer subexpr deeper $"{prefix}q"
            w $"match {prefix}t with"
            w $"| Error -> (parseTreeFromList (List.rev list), {prefix}q)"
            w $"| _ -> z ({prefix}t :: list) {prefix}c"
            writer.Indent <- writer.Indent - 1
            w $"match z [] {param} with"
            w "| (Error, _) -> (Error, q)"
            w "| (Empty, _) -> (Error, q)"
            w "| (Node [], _) -> (Error, q)"
            w "| (tree, next) -> (tree, next)"
        writer.Indent <- writer.Indent - indent
    | Ebnf.Parens subexpr ->
        build writer subexpr depth param
    | Ebnf.StringLiteral s ->
        w $"Parse.isText \"{s}\" {param}"
    | Ebnf.NcName n -> 
        if n = n.ToUpperInvariant() then
            w $"Parse.is{n.Substring(0, 1) + n.Substring(1).ToLowerInvariant()} {param}"
        else
            w $"{n} {param}"

let buildGrammar (grammar:(Map<string, Ebnf.EbnfExpr>)) filename modulename = 
    let writer = new IndentedTextWriter (File.CreateText filename)
    writer.WriteLine $"module {modulename}"
    writer.WriteLine "// Generated code. Do not edit."
    writer.WriteLine "// Doing this instead of parser combinators for more straightforward debugging."
    writer.WriteLine ()
    writer.WriteLine "open Lexer"
    writer.WriteLine "open Parse"
    writer.WriteLine ()
    let mutable prefix = "let rec"

    for production in grammar do
        writer.WriteLine $"{prefix} {production.Key} (q:TokenCursor) ="
        writer.Indent <- writer.Indent + 1
        writer.Write "let result = "
        build writer production.Value 0 "q"
        writer.WriteLine "match result with"
        writer.WriteLine "| (Error, _) -> (Error, q)"
        writer.WriteLine $"| (tree, next) -> (Production (\"{production.Key}\", tree), next)"
        writer.Indent <- writer.Indent - 1
        writer.WriteLine ()
        prefix <- "and"

    writer.Flush ()

let readGrammar filename =
        File.ReadAllLines filename
        |> Seq.filter (fun line -> not (String.IsNullOrWhiteSpace line))
        |> Seq.map Ebnf.parseProduction
        |> Map.ofSeq


type Lexpr = {
    name: String
    ctorArgs: Lexpr list
}

type MonoType = Lexpr list

type TypeDef = {
    foralls: string list
    def: MonoType
}

type TypeDecl = {
    name: string
    typeDef: TypeDef
}

type Pattern =
    | NatP of bigint
    | StrP of string
    | CtorP of string * (Pattern list)

type Expression = {
    assignments: Assignment list
    typeDecls: TypeDecl list
    body: Atom
    arguments: Expression list
    superscript: int
}

and Assignment = {
    lexpr: Lexpr
    body: Expression
}

and Atom = 
    | NatE of bigint
    | StrE of string
    | OperatorE of string
    | LambdaE of Lambda
    | IdE of string
    | CasesE of Cases
    | SyntaxError of string

and Lambda = {
    parameters: Lexpr list
    body: Expression
}

and Cases = {
    arg: Expression
    cases: Case list
}

and Case = {
    pattern: Pattern
    body: Expression
}


let rec dump tree indent isLast =
    let ind = indent + (if isLast then "  " else "│ ")

    match tree with
    | Parse.Token token -> 
        Console.Write indent
        Console.Write (if isLast then "└" else "├")
        Console.Write "─"
        Console.WriteLine (Lexer.tokenStr token)
    | Parse.Node trees ->
        let n = List.length trees
        for (child, i) in List.zip trees [1..n] do
            dump child ind (i = n)
    | Parse.Production (name, tree) -> 
        Console.Write indent
        Console.Write (if isLast then "└" else "├")
        Console.Write "─"
        Console.WriteLine name
        dump tree ind true
    | _ -> Console.WriteLine "???"

let parseSuperscript = Seq.fold (fun acc (ch:char) -> acc * 10 + ("⁰¹²³⁴⁵⁶⁷⁸⁹".IndexOf(ch))) 0

let syntaxError s = { assignments = []; typeDecls = []; body = SyntaxError s; arguments = []; superscript = 1 }

let assignTree tree =
    { lexpr = { name = "SyntaxError"; ctorArgs = [] }; body = syntaxError "Not implemented assignTree" }

let typeDeclTree tree =
    { name = "SyntaxError"; typeDef = { foralls = []; def = [] } }

let atomTree tree =
    SyntaxError "Not implemented atomTree"

let ten = bigint 10

let rec parseNat s n =
    match s with
    | [] -> n
    | ch::rest -> 
        if (ch.ToString() = "_") then
            parseNat rest n
        else
            parseNat rest (n * ten + bigint (Rune.GetNumericValue ch))

let rec argsTree args list =
    match args with
    | [_] -> List.rev list
    | [_; arg]::rest -> argsTree rest ((exprTree arg)::list)
    | _ -> [syntaxError "Not implemented argsTree"]

and exprTrees trees (assigns:Assignment list) (typeDecls:TypeDecl list) =
    match trees with
    | [] -> syntaxError "Expected body in exprTrees"
    | t::ts ->
        match t with
        | Parse.Production (name, tree') when name = "assign" ->
            exprTrees ts ((assignTree t)::assigns) typeDecls
        | Parse.Production (name, tree') when name = "typeDecl" ->
            exprTrees ts assigns ((typeDeclTree t)::typeDecls)
        | Parse.Production (name, tree') when name = "atom" ->
            let expr = { assignments = assigns; typeDecls = typeDecls; body = atomTree t; arguments = []; superscript = 1 }
            match ts with
            | [] -> expr
            | [Parse.Token (Lexer.Superscript ss)] -> { expr with superscript = parseSuperscript (Lexer.spanned ss) }
            | [Parse.Production ("args", Parse.Node args)] -> { expr with arguments = argsTree args [] }
            | _ -> syntaxError "Not implemented exprTrees post-atom"
        | _ -> syntaxError "Not implemented exprTrees"

and exprTree tree =
    match tree with
    | Parse.Node trees -> exprTrees trees [] []
    | Parse.Token token -> 
        match token with
        | Lexer.Nat span -> { assignments = []; typeDecls = []; body = NatE (parseNat ((Lexer.spanned span).EnumerateRunes() |> Seq.toList) (bigint 0)); arguments = []; superscript = 1 }
        | _ -> syntaxError "Not implemented exprTree match"
    | Parse.Production (name, tree') -> exprTree tree'
    | _ -> syntaxError "Not implemented exprTree _"

let syntaxTree tree =
    match tree with
    | Parse.Production (name, tree') ->
        match name with
        | "expr" -> exprTree tree'
        | _ -> syntaxError "Expected an expression"
    | _ -> syntaxError "Missing production name"

let main =

    let coreGrammar = readGrammar "core.grammar.txt"
    buildGrammar coreGrammar "Core.fs" "Core"
    let grammar = readGrammar "grammar.txt"
    buildGrammar grammar "Arafel.fs" "Arafel"

    Console.OutputEncoding <- Encoding.Unicode
    let src = File.ReadAllText("sample.af")
    let cursor = Lexer.makeCursor src
    let tokens = Lexer.tokenise cursor |> Seq.toArray
    let mutable tc = { Lexer.TokenCursor.source = tokens; Lexer.TokenCursor.index = 0 }

    while tc.More do
        let result = Arafel.expr tc
        match result with
        | (Parse.Error, _) -> raise (Exception $"failed at token {tc.index}")
        | (tree, next) -> 
            Console.WriteLine ()
            dump tree "" true
            let syntax = syntaxTree tree
            tc <- next
        
    0