module Tester
// Generated code. Do not edit.

open System
open Random
open Syntax

let rec randomAtom (rand: Random) depth =
    match rand.Next(14) with
    | 0 ->
        let fn = randomAtom rand (depth + 1)
        let args = mkNonempty randomExpr rand (depth + 1)
        CallE(fn, args)
    | 1 ->
        let atom = randomAtom rand (depth + 1)
        let exponent = mkBigint rand (depth + 1)
        ExponentE(atom, exponent)
    | 2 ->
        let value = mkBigint rand (depth + 1)
        NatE(value)
    | 3 ->
        let value = mkString rand (depth + 1)
        StringE(value)
    | 4 ->
        let value = mkBool rand (depth + 1)
        BoolE(value)
    | 5 ->
        let symbol = mkString rand (depth + 1)
        OperatorE(symbol)
    | 6 ->
        let lambda = randomLambda rand (depth + 1)
        LambdaE(lambda)
    | 7 ->
        let expr = randomExpr rand (depth + 1)
        ParensE(expr)
    | 8 ->
        let name = mkString rand (depth + 1)
        IdentifierE(name)
    | 9 ->
        let cases = randomCases rand (depth + 1)
        CasesE(cases)
    | 10 ->
        let ifthen = randomIfThen rand (depth + 1)
        IfThenE(ifthen)
    | 11 ->
        let letDecl = randomLetDecl rand (depth + 1)
        LetE(letDecl)
    | 12 ->
        let typeDecl = randomTypeDecl rand (depth + 1)
        TypeE(typeDecl)
    | _ ->
        let atom = randomAtom rand (depth + 1)
        let terms = mkNonempty randomTerm rand (depth + 1)
        SumE(atom, terms)

and randomCase (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let pattern = randomPattern rand (depth + 1)
        let expr = randomExpr rand (depth + 1)
        Case(pattern, expr)

and randomCases (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let expr = randomExpr rand (depth + 1)
        let cases = mkNonempty randomCase rand (depth + 1)
        let otherwise = mkOption randomExpr rand (depth + 1)
        Cases(expr, cases, otherwise)

and randomElseIf (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let condition = randomExpr rand (depth + 1)
        let trueExpr = randomExpr rand (depth + 1)
        ElseIf(condition, trueExpr)

and randomExpr (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let atom = randomAtom rand (depth + 1)
        Expr(atom)

and randomIfThen (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let condition = randomExpr rand (depth + 1)
        let trueExpr = randomExpr rand (depth + 1)
        let elseifs = mkList randomElseIf rand (depth + 1)
        let falseExpr = randomExpr rand (depth + 1)
        IfThen(condition, trueExpr, elseifs, falseExpr)

and randomLambda (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let name = randomLexpr rand (depth + 1)
        let expr = randomExpr rand (depth + 1)
        Lambda(name, expr)

and randomLetDecl (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let name = randomLexpr rand (depth + 1)
        let expr = randomExpr rand (depth + 1)
        let inExpr = randomExpr rand (depth + 1)
        LetDecl(name, expr, inExpr)

and randomLexpr (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let name = randomLexprName rand (depth + 1)
        let parameters = mkList randomLexpr rand (depth + 1)
        Lexpr(name, parameters)

and randomLexprName (rand: Random) depth =
    match rand.Next(2) with
    | 0 ->
        let name = mkString rand (depth + 1)
        IdentifierN(name)
    | _ ->
        let symbol = mkString rand (depth + 1)
        OperatorN(symbol)

and randomMonoType (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let types = mkNonempty randomLexpr rand (depth + 1)
        MonoType(types)

and randomPattern (rand: Random) depth =
    match rand.Next(4) with
    | 0 ->
        let ctor = mkString rand (depth + 1)
        let args = mkList randomPattern rand (depth + 1)
        CtorPat(ctor, args)
    | 1 ->
        let value = mkBigint rand (depth + 1)
        NatPat(value)
    | 2 ->
        let value = mkString rand (depth + 1)
        StringPat(value)
    | _ ->
        let value = mkBool rand (depth + 1)
        BoolPat(value)

and randomPolyType (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let foralls = mkList mkString rand (depth + 1)
        let cases = mkNonempty randomMonoType rand (depth + 1)
        PolyType(foralls, cases)

and randomTerm (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let operator = mkString rand (depth + 1)
        let atom = randomAtom rand (depth + 1)
        Term(operator, atom)

and randomTypeDecl (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let name = mkString rand (depth + 1)
        let parameters = mkList mkString rand (depth + 1)
        let ptype = randomPolyType rand (depth + 1)
        let inExpr = randomExpr rand (depth + 1)
        TypeDecl(name, parameters, ptype, inExpr)
