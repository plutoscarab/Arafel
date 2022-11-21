module Tester
// Generated code. Do not edit.

open System
open Random
open Syntax

let rec randomAtom (rand: Random) depth =
    match rand.Next(10) with
    | 0 ->
        let value = mkBigint rand (depth + 1)
        NatA(value)
    | 1 ->
        let value = mkString rand (depth + 1)
        StringA(value)
    | 2 ->
        let symbol = mkString rand (depth + 1)
        OperatorA(symbol)
    | 3 ->
        let lambda = randomLambda rand (depth + 1)
        LambdaA(lambda)
    | 4 ->
        let expr = randomExpr rand (depth + 1)
        ParensA(expr)
    | 5 ->
        let name = mkString rand (depth + 1)
        IdentifierA(name)
    | 6 ->
        let cases = randomCases rand (depth + 1)
        CasesA(cases)
    | 7 ->
        let ifthen = randomIfThen rand (depth + 1)
        IfThenA(ifthen)
    | 8 ->
        let letDecl = randomLetDecl rand (depth + 1)
        LetA(letDecl)
    | _ ->
        let typeDecl = randomTypeDecl rand (depth + 1)
        TypeA(typeDecl)

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
        let args = mkList randomExpr rand (depth + 1)
        let post = mkList randomPostfix rand (depth + 1)
        Expr(atom, args, post)

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

and randomPostfix (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let value = mkBigint rand (depth + 1)
        SuperscriptPF(value)

and randomTypeDecl (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let name = mkString rand (depth + 1)
        let parameters = mkList mkString rand (depth + 1)
        let ptype = randomPolyType rand (depth + 1)
        let inExpr = randomExpr rand (depth + 1)
        TypeDecl(name, parameters, ptype, inExpr)
