module Visitor
// Generated code. Do not edit.

open System
open Syntax

type Visitor() =
    
    abstract member Case_Case: Pattern * Expr -> Case
    default this.Case_Case(pattern, expr) =
        let pattern' = this.VisitPattern pattern
        let expr' = this.VisitExpr expr
        Case (pattern', expr')
    
    abstract member VisitCase: Case -> Case
    default this.VisitCase(value) =
        match value with
        | Case (pattern, expr) -> this.Case_Case(pattern, expr)
    
    abstract member Cases_Cases: Expr * Case list * Expr option -> Cases
    default this.Cases_Cases(expr, cases, otherwise) =
        let expr' = this.VisitExpr expr
        let cases' = List.map this.VisitCase cases
        let otherwise' = Option.map this.VisitExpr otherwise
        Cases (expr', cases', otherwise')
    
    abstract member VisitCases: Cases -> Cases
    default this.VisitCases(value) =
        match value with
        | Cases (expr, cases, otherwise) -> this.Cases_Cases(expr, cases, otherwise)
    
    abstract member ElseIf_ElseIf: Expr * Expr -> ElseIf
    default this.ElseIf_ElseIf(condition, trueExpr) =
        let condition' = this.VisitExpr condition
        let trueExpr' = this.VisitExpr trueExpr
        ElseIf (condition', trueExpr')
    
    abstract member VisitElseIf: ElseIf -> ElseIf
    default this.VisitElseIf(value) =
        match value with
        | ElseIf (condition, trueExpr) -> this.ElseIf_ElseIf(condition, trueExpr)
    
    abstract member Expr_CallE: Expr * Expr list -> Expr
    default this.Expr_CallE(fn, args) =
        let fn' = this.VisitExpr fn
        let args' = List.map this.VisitExpr args
        CallE (fn', args')
    
    abstract member Expr_ExponentE: Expr * bigint -> Expr
    default this.Expr_ExponentE(expr, exponent) =
        let expr' = this.VisitExpr expr
        let exponent' = exponent
        ExponentE (expr', exponent')
    
    abstract member Expr_NatE: bigint -> Expr
    default this.Expr_NatE(value) =
        let value' = value
        NatE (value')
    
    abstract member Expr_StringE: string -> Expr
    default this.Expr_StringE(value) =
        let value' = value
        StringE (value')
    
    abstract member Expr_BoolE: bool -> Expr
    default this.Expr_BoolE(value) =
        let value' = value
        BoolE (value')
    
    abstract member Expr_OperatorE: string -> Expr
    default this.Expr_OperatorE(symbol) =
        let symbol' = symbol
        OperatorE (symbol')
    
    abstract member Expr_LambdaE: Lambda -> Expr
    default this.Expr_LambdaE(lambda) =
        let lambda' = this.VisitLambda lambda
        LambdaE (lambda')
    
    abstract member Expr_ParensE: Expr -> Expr
    default this.Expr_ParensE(expr) =
        let expr' = this.VisitExpr expr
        ParensE (expr')
    
    abstract member Expr_IdentifierE: string -> Expr
    default this.Expr_IdentifierE(name) =
        let name' = name
        IdentifierE (name')
    
    abstract member Expr_CasesE: Cases -> Expr
    default this.Expr_CasesE(cases) =
        let cases' = this.VisitCases cases
        CasesE (cases')
    
    abstract member Expr_IfThenE: IfThen -> Expr
    default this.Expr_IfThenE(ifthen) =
        let ifthen' = this.VisitIfThen ifthen
        IfThenE (ifthen')
    
    abstract member Expr_LetE: LetDecl -> Expr
    default this.Expr_LetE(letDecl) =
        let letDecl' = this.VisitLetDecl letDecl
        LetE (letDecl')
    
    abstract member Expr_TypeE: TypeDecl -> Expr
    default this.Expr_TypeE(typeDecl) =
        let typeDecl' = this.VisitTypeDecl typeDecl
        TypeE (typeDecl')
    
    abstract member Expr_SumE: Expr * Term list -> Expr
    default this.Expr_SumE(expr, terms) =
        let expr' = this.VisitExpr expr
        let terms' = List.map this.VisitTerm terms
        SumE (expr', terms')
    
    abstract member VisitExpr: Expr -> Expr
    default this.VisitExpr(value) =
        match value with
        | CallE (fn, args) -> this.Expr_CallE(fn, args)
        | ExponentE (expr, exponent) -> this.Expr_ExponentE(expr, exponent)
        | NatE (value) -> this.Expr_NatE(value)
        | StringE (value) -> this.Expr_StringE(value)
        | BoolE (value) -> this.Expr_BoolE(value)
        | OperatorE (symbol) -> this.Expr_OperatorE(symbol)
        | LambdaE (lambda) -> this.Expr_LambdaE(lambda)
        | ParensE (expr) -> this.Expr_ParensE(expr)
        | IdentifierE (name) -> this.Expr_IdentifierE(name)
        | CasesE (cases) -> this.Expr_CasesE(cases)
        | IfThenE (ifthen) -> this.Expr_IfThenE(ifthen)
        | LetE (letDecl) -> this.Expr_LetE(letDecl)
        | TypeE (typeDecl) -> this.Expr_TypeE(typeDecl)
        | SumE (expr, terms) -> this.Expr_SumE(expr, terms)
    
    abstract member IfThen_IfThen: Expr * Expr * ElseIf list * Expr -> IfThen
    default this.IfThen_IfThen(condition, trueExpr, elseifs, falseExpr) =
        let condition' = this.VisitExpr condition
        let trueExpr' = this.VisitExpr trueExpr
        let elseifs' = List.map this.VisitElseIf elseifs
        let falseExpr' = this.VisitExpr falseExpr
        IfThen (condition', trueExpr', elseifs', falseExpr')
    
    abstract member VisitIfThen: IfThen -> IfThen
    default this.VisitIfThen(value) =
        match value with
        | IfThen (condition, trueExpr, elseifs, falseExpr) -> this.IfThen_IfThen(condition, trueExpr, elseifs, falseExpr)
    
    abstract member Lambda_Lambda: Lexpr * Expr -> Lambda
    default this.Lambda_Lambda(name, expr) =
        let name' = this.VisitLexpr name
        let expr' = this.VisitExpr expr
        Lambda (name', expr')
    
    abstract member VisitLambda: Lambda -> Lambda
    default this.VisitLambda(value) =
        match value with
        | Lambda (name, expr) -> this.Lambda_Lambda(name, expr)
    
    abstract member LetDecl_LetDecl: Lexpr * Expr * Expr -> LetDecl
    default this.LetDecl_LetDecl(name, expr, inExpr) =
        let name' = this.VisitLexpr name
        let expr' = this.VisitExpr expr
        let inExpr' = this.VisitExpr inExpr
        LetDecl (name', expr', inExpr')
    
    abstract member VisitLetDecl: LetDecl -> LetDecl
    default this.VisitLetDecl(value) =
        match value with
        | LetDecl (name, expr, inExpr) -> this.LetDecl_LetDecl(name, expr, inExpr)
    
    abstract member Lexpr_Lexpr: LexprName * Lexpr list -> Lexpr
    default this.Lexpr_Lexpr(name, parameters) =
        let name' = this.VisitLexprName name
        let parameters' = List.map this.VisitLexpr parameters
        Lexpr (name', parameters')
    
    abstract member VisitLexpr: Lexpr -> Lexpr
    default this.VisitLexpr(value) =
        match value with
        | Lexpr (name, parameters) -> this.Lexpr_Lexpr(name, parameters)
    
    abstract member LexprName_IdentifierN: string -> LexprName
    default this.LexprName_IdentifierN(name) =
        let name' = name
        IdentifierN (name')
    
    abstract member LexprName_OperatorN: string -> LexprName
    default this.LexprName_OperatorN(symbol) =
        let symbol' = symbol
        OperatorN (symbol')
    
    abstract member VisitLexprName: LexprName -> LexprName
    default this.VisitLexprName(value) =
        match value with
        | IdentifierN (name) -> this.LexprName_IdentifierN(name)
        | OperatorN (symbol) -> this.LexprName_OperatorN(symbol)
    
    abstract member MonoType_MonoType: Lexpr list -> MonoType
    default this.MonoType_MonoType(types) =
        let types' = List.map this.VisitLexpr types
        MonoType (types')
    
    abstract member VisitMonoType: MonoType -> MonoType
    default this.VisitMonoType(value) =
        match value with
        | MonoType (types) -> this.MonoType_MonoType(types)
    
    abstract member Pattern_CtorPat: string * Pattern list -> Pattern
    default this.Pattern_CtorPat(ctor, args) =
        let ctor' = ctor
        let args' = List.map this.VisitPattern args
        CtorPat (ctor', args')
    
    abstract member Pattern_NatPat: bigint -> Pattern
    default this.Pattern_NatPat(value) =
        let value' = value
        NatPat (value')
    
    abstract member Pattern_StringPat: string -> Pattern
    default this.Pattern_StringPat(value) =
        let value' = value
        StringPat (value')
    
    abstract member Pattern_BoolPat: bool -> Pattern
    default this.Pattern_BoolPat(value) =
        let value' = value
        BoolPat (value')
    
    abstract member VisitPattern: Pattern -> Pattern
    default this.VisitPattern(value) =
        match value with
        | CtorPat (ctor, args) -> this.Pattern_CtorPat(ctor, args)
        | NatPat (value) -> this.Pattern_NatPat(value)
        | StringPat (value) -> this.Pattern_StringPat(value)
        | BoolPat (value) -> this.Pattern_BoolPat(value)
    
    abstract member PolyType_PolyType: string list * MonoType list -> PolyType
    default this.PolyType_PolyType(foralls, cases) =
        let foralls' = foralls
        let cases' = List.map this.VisitMonoType cases
        PolyType (foralls', cases')
    
    abstract member VisitPolyType: PolyType -> PolyType
    default this.VisitPolyType(value) =
        match value with
        | PolyType (foralls, cases) -> this.PolyType_PolyType(foralls, cases)
    
    abstract member Term_Term: string * Expr -> Term
    default this.Term_Term(operator, expr) =
        let operator' = operator
        let expr' = this.VisitExpr expr
        Term (operator', expr')
    
    abstract member VisitTerm: Term -> Term
    default this.VisitTerm(value) =
        match value with
        | Term (operator, expr) -> this.Term_Term(operator, expr)
    
    abstract member TypeDecl_TypeDecl: string * string list * PolyType * Expr -> TypeDecl
    default this.TypeDecl_TypeDecl(name, parameters, ptype, inExpr) =
        let name' = name
        let parameters' = parameters
        let ptype' = this.VisitPolyType ptype
        let inExpr' = this.VisitExpr inExpr
        TypeDecl (name', parameters', ptype', inExpr')
    
    abstract member VisitTypeDecl: TypeDecl -> TypeDecl
    default this.VisitTypeDecl(value) =
        match value with
        | TypeDecl (name, parameters, ptype, inExpr) -> this.TypeDecl_TypeDecl(name, parameters, ptype, inExpr)
