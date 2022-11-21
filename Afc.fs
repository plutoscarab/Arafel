
open System
open System.IO

open Cursor
open Lexer
open Localisation
open Parse
open Parser
open Syntax
open Visitor

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

let parseExpressions src =
    let tokens = tokenise Parser.keywords (mkCursor src) |> Seq.toList
    let mutable current = tokens

    seq {
        while current <> [] do
            let (result, next) = parseExpr current
            
            match result with
            | Nomatch e ->
                printfn "%s" (errStr e current)
            | SyntaxError e ->
                printfn "%s" (errStr e next)
            | Match expr ->
                yield expr

            if current <> next then
                current <- next
            else
                current <- List.tail current
    }
    |> Seq.toList

type CoreVisitor() =
    inherit Visitor()

    // Fully curry all the arguments of functions
    override this.LetDecl_LetDecl(name, expr) =
        let name' = this.VisitLexpr name
        let expr' = this.VisitExpr expr

        match name' with
        | Lexpr(n, ps) ->
            let folder p ex =
                Expr ([], LambdaA (Lambda (p, ex)), [], [])
            let lambda = List.foldBack folder ps expr'
            LetDecl (Lexpr(n, []), lambda)

    // Fully curry all the arguments of function applications
    override this.Expr_Expr(prelude, atom, args, post) =
        let folder ex arg =
            Expr ([], ParensA (ex), [arg], [])
        let zero =
            Expr ([], atom, [], [])
        match List.fold folder zero args with
        | Expr(_, at, ar, _) ->
            let prelude' = List.map this.VisitPrelude prelude
            let atom' = this.VisitAtom at
            let args' = List.map this.VisitExpr ar
            let post' = List.map this.VisitPostfix post

            // Parenthesis elimination
            let mutable result = Expr (prelude', atom', args', post')

            match atom' with
            | ParensA pexpr ->
                match pexpr with
                | Expr (ppre, patom, pargs, ppost) ->
                    if (ppre = [] || prelude' = []) 
                        && (pargs = [] || args' = [])
                        && (ppost = [] || post' = [])
                    then
                        result <- Expr (prelude' @ ppre, patom, args' @ pargs, post' @ ppost) 
            | _ -> ignore ()

            result
    
    // Replace if-then with case
    override this.Atom_IfThenA(ifthen) =
        let ifthen' = this.VisitIfThen ifthen
        match ifthen' with
        | IfThen(condition, trueExpr, elseifs, falseExpr) ->
            let folder (ElseIf (co, th)) expr =
                let cs = CasesA (Cases (co, [
                    Case (BoolPat (true), th)
                ], Some expr))
                Expr ([], cs, [], [])
            let elseExpr = List.foldBack folder elseifs falseExpr
            CasesA (Cases (condition, [
                Case (BoolPat (true), trueExpr)
            ], Some elseExpr))

let pretty filename exprs =
    Directory.CreateDirectory "output" |> ignore
    use file = File.CreateText $"output/{filename}"
    use writer = new System.CodeDom.Compiler.IndentedTextWriter(file)
    writer.Indent <- 1
    
    for expr in exprs do
        writer.WriteLine "\r\n"
        Pretty.printExpr writer expr

[<EntryPoint>]
let main (args: string[]) =

    let src = File.ReadAllText (args.[0])
    let exprs = parseExpressions src
    pretty "pretty.af" exprs

    let visitor = CoreVisitor()
    let cexprs = List.map visitor.VisitExpr exprs
    pretty "core.af" cexprs

    0