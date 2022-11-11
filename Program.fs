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
        for ((item, seqName), i) in List.zip list [1..List.length(list)] do
            v $"let ({prefix}t{i}, {prefix}c{i}) = "
            build writer item deeper param
            w $"if {prefix}t{i} <> Error then ({prefix}t{i}, {prefix}c{i}) else"
        let items = [1..List.length(list)] |> List.map (fun i -> $"{prefix}t{i}") |> String.concat "; "
        w $"(Error, {param})"
        writer.Indent <- writer.Indent - indent
    | Ebnf.Sequence list | Ebnf.Fields list ->
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

type Postfix = Superscript of bigint

type Expression = 
    { assignments: Assignment list
      typeDecls: TypeDecl list
      body: Atom
      arguments: Expression list
      postfixes: Postfix list }

      member this.withPostfix(postfix:Postfix) =
          { this with postfixes = postfix::this.postfixes }

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
    | IfThenE of Expression * Expression * Expression
    | SyntaxError of string

and Lambda = {
    parameters: Lexpr list
    body: Expression
}

and Cases = {
    arg: Expression
    cases: Case list
    otherwise: Expression option
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

let ten =
    bigint 10

let parseSuperscript s =
    Superscript (
        Seq.fold
            (fun acc (ch:char) -> acc * ten + (bigint ("⁰¹²³⁴⁵⁶⁷⁸⁹".IndexOf(ch)))) 
            (bigint 0)
            s
        )

let rec parseNat' s n =
    match s with
    | [] -> n
    | ch::rest ->
        if (ch.ToString() = "_") then
            parseNat' rest n
        else
            parseNat' rest (n * ten + bigint (Rune.GetNumericValue ch))

let parseNat span =
    parseNat' ((Lexer.spanned span).EnumerateRunes() |> Seq.toList) (bigint 0)

let fromAtom atom = {
    assignments = []
    typeDecls = []
    body = atom
    arguments = []
    postfixes = []
}

let syntaxError s =
    fromAtom (SyntaxError s)

let rec parseParensList parser list =
    match list with
    | [_] -> []
    | _::arg::rest -> (parseParensList parser rest) @ [parser arg]
    | _ -> raise (Exception "Implementation error for parseParensList")

let rec lexprTree' lexpr =
    match lexpr with
    | Parse.Token token ->
        { name = Lexer.tokenText token; ctorArgs = [] }
    | Parse.Node ((Parse.Token name)::args) ->
        { name = Lexer.tokenText name; ctorArgs = parseParensList lexprTree' args }
    | Parse.Production (name, lexpr') ->
        lexprTree' lexpr'
    | _ ->
        { name = "SyntaxError"; ctorArgs = [] }

let lexprTree tree =
    match tree with
    | Parse.Production ("lexpr", lexpr) ->
        lexprTree' lexpr
    | _ -> { name = "SyntaxError"; ctorArgs = [] }

let typeDeclTree tree = {
    name = "SyntaxError"
    typeDef = { foralls = []; def = [] } 
    }

let rec atomTree' atom =
    match atom with
    | Parse.Token token ->
        match token with
        | Lexer.Operator span -> OperatorE (Lexer.spanned span)
        | Lexer.Nat span -> NatE (parseNat span)
        | Lexer.Id span -> IdE (Lexer.spanned span)
        | _ -> SyntaxError "Not implemented atomTree' token"
    | Parse.Production ("cases", Parse.Node trees) ->
        caseTrees trees
    | Parse.Production ("ifthen", Parse.Node [_; arg; _; trueExpr; _; falseExpr]) ->
        IfThenE (syntaxTree arg, syntaxTree trueExpr, syntaxTree falseExpr)
    | _ -> SyntaxError "Not implemented atomTree'"

and atomTree tree =
    match tree with
    | Parse.Production ("atom", atom) -> atomTree' atom
    | _ -> SyntaxError "Not implemented atomTree"

and assignTree' assign =
    match assign with
    | Parse.Node [_; lexpr; _; body] -> { lexpr = lexprTree lexpr; body = syntaxTree body }
    | _ -> {
        lexpr = {
            name = "SyntaxError"
            ctorArgs = []
            }
        body = syntaxError "Not implemented assignTree'" 
        }

and assignTree tree =
    match tree with
    | Parse.Production ("assign", assign) ->
        assignTree' assign
    | _ -> {
        lexpr = {
            name = "SyntaxError"
            ctorArgs = []
            }
        body = syntaxError "Not implemented assignTree" 
        }

and argsTree args list =
    match args with
    | [_] ->
        List.rev list
    | _::arg::rest ->
        argsTree rest ((syntaxTree arg)::list)
    | _ -> [syntaxError "Not implemented argsTree"]

and patternTree' tree =
    match tree with
    | Parse.Token (Lexer.Nat s) ->
        NatP (parseNat s)
    | Parse.Token (Lexer.String s) ->
        StrP (Lexer.spanned s)
    | Parse.Token (Lexer.Id s) ->
        CtorP (Lexer.spanned s, [])
    | Parse.Node ((Parse.Token (Lexer.Id s))::rest) ->
        CtorP (Lexer.spanned s, parseParensList patternTree rest)
    | _ ->
        raise (Exception "Not implemented patternTree'")

and patternTree tree =
    match tree with
    | Parse.Production ("pattern", pattern) ->
        patternTree' pattern
    | _ -> raise (Exception "Not implemented patternTree")

and caseTrees' body trees =
    match trees with
    | Parse.Production ("pattern", pattern) ::
      Parse.Token (Lexer.Operator _) ::
      Parse.Production ("expr", tree) :: [] ->
        { arg = body; cases = [{ pattern = patternTree' pattern; body = exprTree tree }]; otherwise = None }
    | Parse.Production ("pattern", pattern) ::
      Parse.Token (Lexer.Operator _) ::
      Parse.Production ("expr", tree) ::rest ->
        let c = caseTrees' body rest
        { c with cases = { pattern = patternTree' pattern; body = exprTree tree }::c.cases }
    | (Parse.Token (Lexer.Id span)) :: tree :: [] when (Lexer.spanned span) = "otherwise" ->
        { arg = body; cases = []; otherwise = Some (syntaxTree tree) }
    | _ -> raise (Exception "Not implemented caseTrees'")

and caseTrees trees =
    match trees with
    | Parse.Token (Lexer.Keyword s) :: Parse.Production ("expr", expr) :: rest 
        when (Lexer.spanned s) = "case" ->
        CasesE (caseTrees' (exprTree expr) rest)
    | _ ->
        raise (Exception "Not implemented caseTrees")

and exprTrees trees assigns typeDecls =
    match trees with
    | [] -> syntaxError "Expected body in exprTrees"
    | t::ts ->
        match t with
        | Parse.Production (name, tree') when name = "assign" ->
            exprTrees ts ((assignTree t)::assigns) typeDecls
        | Parse.Production (name, tree') when name = "typeDecl" ->
            exprTrees ts assigns ((typeDeclTree t)::typeDecls)
        | Parse.Production (name, tree') when name = "atom" ->
            let expr = {
                assignments = assigns
                typeDecls = typeDecls
                body = atomTree t
                arguments = []
                postfixes = []
            }
            match ts with
            | [] ->
                expr
            | [Parse.Token (Lexer.Superscript ss)] ->
                expr.withPostfix (parseSuperscript (Lexer.spanned ss))
            | [Parse.Production ("args", Parse.Node args)] ->
                { expr with arguments = argsTree args [] }
            | _ -> syntaxError "Not implemented exprTrees post-atom"
        | _ -> syntaxError "Not implemented exprTrees"

and exprTree tree =
    match tree with
    | Parse.Node trees -> exprTrees trees [] []
    | Parse.Production ("atom", tree') -> fromAtom (atomTree' tree')
    | _ -> syntaxError "Not implemented exprTree _"

and syntaxTree tree =
    match tree with
    | Parse.Production (name, tree') ->
        match name with
        | "expr" -> exprTree tree'
        | _ -> syntaxError "Expected an expression"
    | _ -> syntaxError "Missing production name"

let rec ctorStr (typeDef, name) =
    "\r\n    | " + 
        match name with
        | None -> typeStr typeDef
        | Some name -> name + " of " + (typeStr typeDef)

and typeStr typeDef =
    match typeDef with
    | Ebnf.Fields list -> list |> List.map typeStr |> String.concat " * "
    | Ebnf.Choice [(t, None)] -> typeStr t
    | Ebnf.Choice list -> list |> List.map ctorStr |> String.concat ""
    | Ebnf.Parens t -> typeStr t
    | Ebnf.StringLiteral s -> raise (Exception "")
    | Ebnf.Sequence list -> Ebnf.getName typeDef
    | Ebnf.Primary (m, t) -> Ebnf.getName typeDef
    | Ebnf.NcName n -> Ebnf.getName typeDef

let rec buildParser (writer:IndentedTextWriter) expr =
    match expr with
    | Ebnf.Choice [(e, None)] ->
        buildParser writer e
    | Ebnf.Fields fields ->
        writer.Indent <- writer.Indent + 1
        writer.WriteLine "parser {"
        writer.Indent <- writer.Indent + 1
        let ids = [0..List.length(fields)-1]

        for (i, field) in List.zip ids fields do
            writer.WriteLine $"let! f{i} ="
            buildParser writer field

        writer.WriteLine ("return " + (String.concat ", " (ids |> List.map (fun i -> $"f{i}"))))
        writer.Indent <- writer.Indent - 1
        writer.WriteLine "}"
        writer.Indent <- writer.Indent - 1
    | Ebnf.Choice choices ->
        writer.Indent <- writer.Indent + 1
        let ids = [0..List.length(choices)-1]

        for (i, choice) in List.zip ids choices do
            writer.WriteLine $"let f{i} () ="
            buildParser writer (fst choice)

        let mutable op = ""

        for (i, choice) in List.zip ids choices do
            let name = 
                match snd choice with
                | None -> ""
                | Some n -> n
            writer.WriteLine $"{op}parser {{"
            writer.WriteLine $"    let! r = f{i}()"
            writer.WriteLine $"    return ({name} r)"
            writer.WriteLine "}"
            op <- "<|> "

        writer.Indent <- writer.Indent - 1
    | _ ->
        writer.WriteLine "    P (fun c -> (None, c))"

let buildTypes grammarFile filename modulename = 
    let grammar = readGrammar grammarFile
    let writer = new IndentedTextWriter (File.CreateText filename)
    writer.WriteLine $"module {modulename}"
    writer.WriteLine $"// Generated code. Do not edit. Make changes in {grammarFile}."
    writer.WriteLine ()
    writer.WriteLine "open Lexer"
    writer.WriteLine "open Parse"
    writer.WriteLine ()
    let mutable t = "type"

    for production in grammar do
        let ctorName =
            match production.Value with
            | Ebnf.Choice ((_, Some _)::_) -> ""
            | _ -> "\r\n    " + production.Key + " of "
        writer.WriteLine $"{t} {production.Key} = {ctorName}{typeStr production.Value}"
        t <- "and"
        writer.WriteLine ()

    t <- "let rec"

    for production in grammar do
        let pname = production.Key.Substring(0, 1).ToLowerInvariant() + production.Key.Substring(1)
        writer.WriteLine $"{t} {pname} () ="
        buildParser writer production.Value
        writer.WriteLine ()
        t <- "and"

    writer.Flush ()
    grammar

let main =

    let languageGrammar = buildTypes "language.txt" "Language.fs" "Language"
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