module Tester
// Generated code. Do not edit.

open System
open Random
open Syntax

let rec randomAtom (rand: Random) depth =
    match rand.Next(9) with
    | 0 ->
        let fn = randomAtom rand (depth + 1)
        let args = mkNonempty randomExpr rand (depth + 1)
        CallA(fn, args)
    | 1 ->
        let atom = randomAtom rand (depth + 1)
        let exponent = mkBigint rand (depth + 1)
        ExponentA(atom, exponent)
    | 2 ->
        let intSeq = randomIntSeq rand (depth + 1)
        IntSeqA(intSeq)
    | 3 ->
        let value = mkBigint rand (depth + 1)
        NatA(value)
    | 4 ->
        let value = mkString rand (depth + 1)
        StringA(value)
    | 5 ->
        let value = mkBool rand (depth + 1)
        BoolA(value)
    | 6 ->
        let symbol = mkString rand (depth + 1)
        OperatorA(symbol)
    | 7 ->
        let expr = randomExpr rand (depth + 1)
        ParensA(expr)
    | _ ->
        let name = mkString rand (depth + 1)
        IdentifierA(name)

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
    match rand.Next(7) with
    | 0 ->
        let term = randomUnatom rand (depth + 1)
        let terms = mkList randomTerm rand (depth + 1)
        Expr(term, terms)
    | 1 ->
        let typeDecl = randomTypeDecl rand (depth + 1)
        TypeE(typeDecl)
    | 2 ->
        let letDecl = randomLetDecl rand (depth + 1)
        LetE(letDecl)
    | 3 ->
        let cases = randomCases rand (depth + 1)
        CasesE(cases)
    | 4 ->
        let ifthen = randomIfThen rand (depth + 1)
        IfThenE(ifthen)
    | 5 ->
        let name = randomLexpr rand (depth + 1)
        let expr = randomExpr rand (depth + 1)
        LambdaE(name, expr)
    | _ ->
        let expr = randomExpr rand (depth + 1)
        CurlyLambdaE(expr)

and randomIfThen (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let condition = randomExpr rand (depth + 1)
        let trueExpr = randomExpr rand (depth + 1)
        let elseifs = mkList randomElseIf rand (depth + 1)
        let falseExpr = randomExpr rand (depth + 1)
        IfThen(condition, trueExpr, elseifs, falseExpr)

and randomIntSeq (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let ranges = mkList randomRange rand (depth + 1)
        IntSeq(ranges)

and randomLetDecl (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let name = randomLexpr rand (depth + 1)
        let expr = randomExpr rand (depth + 1)
        let inExpr = mkOption randomExpr rand (depth + 1)
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

and randomRange (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let first = mkBigint rand (depth + 1)
        let last = mkOption mkBigint rand (depth + 1)
        let skip = mkOption mkBigint rand (depth + 1)
        Range(first, last, skip)

and randomTerm (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let operator = mkString rand (depth + 1)
        let atom = randomUnatom rand (depth + 1)
        Term(operator, atom)

and randomTypeDecl (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let name = mkString rand (depth + 1)
        let parameters = mkList mkString rand (depth + 1)
        let ptype = randomPolyType rand (depth + 1)
        let inExpr = randomExpr rand (depth + 1)
        TypeDecl(name, parameters, ptype, inExpr)

and randomUnatom (rand: Random) depth =
    match rand.Next(1) with
    | _ ->
        let operator = mkOption mkString rand (depth + 1)
        let atom = randomAtom rand (depth + 1)
        Unatom(operator, atom)
