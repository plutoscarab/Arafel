module Tester

open System
open Random
open Syntax

let rec randomAtom (rand: Random) depth =
    match rand.Next(8) with
    | 0 ->
        let f0 = mkBigint rand (depth + 1)
        NatA(f0)
    | 1 ->
        let f0 = mkString rand (depth + 1)
        StringA(f0)
    | 2 ->
        let f0 = mkString rand (depth + 1)
        OperatorA(f0)
    | 3 ->
        let f0 = randomLambda rand (depth + 1)
        LambdaA(f0)
    | 4 ->
        let f0 = randomExpr rand (depth + 1)
        ParensA(f0)
    | 5 ->
        let f0 = mkString rand (depth + 1)
        IdentifierA(f0)
    | 6 ->
        let f0 = randomCases rand (depth + 1)
        CasesA(f0)
    | _ ->
        let f0 = randomIfThen rand (depth + 1)
        IfThenA(f0)

and randomCase (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = randomPattern rand (depth + 1)
        let f1 = randomExpr rand (depth + 1)
        Case(f0, f1)

and randomCases (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = randomExpr rand (depth + 1)
        let f1 = mkNonempty randomCase rand (depth + 1)
        let f2 = mkOption randomExpr rand (depth + 1)
        Cases(f0, f1, f2)

and randomCommand (rand: Random) depth =
    match rand.Next(3) with
    | 0 ->
        let f0 = randomTypeDecl rand (depth + 1)
        TypeCmd(f0)
    | 1 ->
        let f0 = randomLetDecl rand (depth + 1)
        LetCmd(f0)
    | _ ->
        let f0 = randomExpr rand (depth + 1)
        ExprCmd(f0)

and randomElseIf (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = randomExpr rand (depth + 1)
        let f1 = randomExpr rand (depth + 1)
        ElseIf(f0, f1)

and randomExpr (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = mkList randomPrelude rand (depth + 1)
        let f1 = randomAtom rand (depth + 1)
        let f2 = mkList randomExpr rand (depth + 1)
        let f3 = mkList randomPostfix rand (depth + 1)
        Expr(f0, f1, f2, f3)

and randomIfThen (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = randomExpr rand (depth + 1)
        let f1 = randomExpr rand (depth + 1)
        let f2 = mkList randomElseIf rand (depth + 1)
        let f3 = randomExpr rand (depth + 1)
        IfThen(f0, f1, f2, f3)

and randomLambda (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = randomPattern rand (depth + 1)
        let f1 = randomExpr rand (depth + 1)
        Lambda(f0, f1)

and randomLetDecl (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = randomLexpr rand (depth + 1)
        let f1 = randomExpr rand (depth + 1)
        LetDecl(f0, f1)

and randomLexpr (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = randomLexprName rand (depth + 1)
        let f1 = mkList randomLexpr rand (depth + 1)
        Lexpr(f0, f1)

and randomLexprName (rand: Random) depth =
    match rand.Next(2) with
    | 0 ->
        let f0 = mkString rand (depth + 1)
        IdentifierN(f0)
    | _ ->
        let f0 = mkString rand (depth + 1)
        OperatorN(f0)

and randomMonoType (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = mkNonempty randomLexpr rand (depth + 1)
        MonoType(f0)

and randomPattern (rand: Random) depth =
    match rand.Next(3) with
    | 0 ->
        let f0 = mkString rand (depth + 1)
        let f1 = mkList randomPattern rand (depth + 1)
        CtorPat(f0, f1)
    | 1 ->
        let f0 = mkBigint rand (depth + 1)
        NatPat(f0)
    | _ ->
        let f0 = mkString rand (depth + 1)
        StringPat(f0)

and randomPolyType (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = mkList mkString rand (depth + 1)
        let f1 = mkNonempty randomMonoType rand (depth + 1)
        PolyType(f0, f1)

and randomPostfix (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = mkBigint rand (depth + 1)
        SuperscriptPF(f0)

and randomPrelude (rand: Random) depth =
    match rand.Next(2) with
    | 0 ->
        let f0 = randomTypeDecl rand (depth + 1)
        TypeP(f0)
    | _ ->
        let f0 = randomLetDecl rand (depth + 1)
        LetP(f0)

and randomTypeDecl (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let f0 = mkString rand (depth + 1)
        let f1 = mkList mkString rand (depth + 1)
        let f2 = randomPolyType rand (depth + 1)
        TypeDecl(f0, f1, f2)
