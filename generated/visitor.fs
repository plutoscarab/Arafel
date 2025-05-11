module Visitor
// Generated code. Do not edit.

open System
open Syntax

type Visitor() =
    
    abstract member Atom_CallA: Atom * Expr list -> Atom
    default this.Atom_CallA(fn, args) =
        let fn' = this.VisitAtom fn
        let args' = List.map this.VisitExpr args
        CallA (fn', args')
    
    abstract member Atom_ExponentA: Atom * bigint -> Atom
    default this.Atom_ExponentA(atom, exponent) =
        let atom' = this.VisitAtom atom
        let exponent' = exponent
        ExponentA (atom', exponent')
    
    abstract member Atom_IntSeqA: IntSeq -> Atom
    default this.Atom_IntSeqA(intSeq) =
        let intSeq' = this.VisitIntSeq intSeq
        IntSeqA (intSeq')
    
    abstract member Atom_NatA: bigint -> Atom
    default this.Atom_NatA(value) =
        let value' = value
        NatA (value')
    
    abstract member Atom_StringA: string -> Atom
    default this.Atom_StringA(value) =
        let value' = value
        StringA (value')
    
    abstract member Atom_BoolA: bool -> Atom
    default this.Atom_BoolA(value) =
        let value' = value
        BoolA (value')
    
    abstract member Atom_OperatorA: string -> Atom
    default this.Atom_OperatorA(symbol) =
        let symbol' = symbol
        OperatorA (symbol')
    
    abstract member Atom_ParensA: Expr -> Atom
    default this.Atom_ParensA(expr) =
        let expr' = this.VisitExpr expr
        ParensA (expr')
    
    abstract member Atom_IdentifierA: string -> Atom
    default this.Atom_IdentifierA(name) =
        let name' = name
        IdentifierA (name')
    
    abstract member VisitAtom: Atom -> Atom
    default this.VisitAtom(value) =
        match value with
        | CallA (fn, args) -> this.Atom_CallA(fn, args)
        | ExponentA (atom, exponent) -> this.Atom_ExponentA(atom, exponent)
        | IntSeqA (intSeq) -> this.Atom_IntSeqA(intSeq)
        | NatA (value) -> this.Atom_NatA(value)
        | StringA (value) -> this.Atom_StringA(value)
        | BoolA (value) -> this.Atom_BoolA(value)
        | OperatorA (symbol) -> this.Atom_OperatorA(symbol)
        | ParensA (expr) -> this.Atom_ParensA(expr)
        | IdentifierA (name) -> this.Atom_IdentifierA(name)
    
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
    
    abstract member Expr_Expr: Unatom * Term list -> Expr
    default this.Expr_Expr(term, terms) =
        let term' = this.VisitUnatom term
        let terms' = List.map this.VisitTerm terms
        Expr (term', terms')
    
    abstract member Expr_TypeE: TypeDecl -> Expr
    default this.Expr_TypeE(typeDecl) =
        let typeDecl' = this.VisitTypeDecl typeDecl
        TypeE (typeDecl')
    
    abstract member Expr_LetE: LetDecl -> Expr
    default this.Expr_LetE(letDecl) =
        let letDecl' = this.VisitLetDecl letDecl
        LetE (letDecl')
    
    abstract member Expr_CasesE: Cases -> Expr
    default this.Expr_CasesE(cases) =
        let cases' = this.VisitCases cases
        CasesE (cases')
    
    abstract member Expr_IfThenE: IfThen -> Expr
    default this.Expr_IfThenE(ifthen) =
        let ifthen' = this.VisitIfThen ifthen
        IfThenE (ifthen')
    
    abstract member Expr_LambdaE: Lexpr * Expr -> Expr
    default this.Expr_LambdaE(name, expr) =
        let name' = this.VisitLexpr name
        let expr' = this.VisitExpr expr
        LambdaE (name', expr')
    
    abstract member Expr_CurlyLambdaE: Expr -> Expr
    default this.Expr_CurlyLambdaE(expr) =
        let expr' = this.VisitExpr expr
        CurlyLambdaE (expr')
    
    abstract member VisitExpr: Expr -> Expr
    default this.VisitExpr(value) =
        match value with
        | Expr (term, terms) -> this.Expr_Expr(term, terms)
        | TypeE (typeDecl) -> this.Expr_TypeE(typeDecl)
        | LetE (letDecl) -> this.Expr_LetE(letDecl)
        | CasesE (cases) -> this.Expr_CasesE(cases)
        | IfThenE (ifthen) -> this.Expr_IfThenE(ifthen)
        | LambdaE (name, expr) -> this.Expr_LambdaE(name, expr)
        | CurlyLambdaE (expr) -> this.Expr_CurlyLambdaE(expr)
    
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
    
    abstract member IntSeq_IntSeq: Range list -> IntSeq
    default this.IntSeq_IntSeq(ranges) =
        let ranges' = List.map this.VisitRange ranges
        IntSeq (ranges')
    
    abstract member VisitIntSeq: IntSeq -> IntSeq
    default this.VisitIntSeq(value) =
        match value with
        | IntSeq (ranges) -> this.IntSeq_IntSeq(ranges)
    
    abstract member LetDecl_LetDecl: Lexpr * Expr * Expr option -> LetDecl
    default this.LetDecl_LetDecl(name, expr, inExpr) =
        let name' = this.VisitLexpr name
        let expr' = this.VisitExpr expr
        let inExpr' = Option.map this.VisitExpr inExpr
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
    
    abstract member Range_Range: bigint * bigint option * bigint option -> Range
    default this.Range_Range(first, last, skip) =
        let first' = first
        let last' = last
        let skip' = skip
        Range (first', last', skip')
    
    abstract member VisitRange: Range -> Range
    default this.VisitRange(value) =
        match value with
        | Range (first, last, skip) -> this.Range_Range(first, last, skip)
    
    abstract member Term_Term: string * Unatom -> Term
    default this.Term_Term(operator, atom) =
        let operator' = operator
        let atom' = this.VisitUnatom atom
        Term (operator', atom')
    
    abstract member VisitTerm: Term -> Term
    default this.VisitTerm(value) =
        match value with
        | Term (operator, atom) -> this.Term_Term(operator, atom)
    
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
    
    abstract member Unatom_Unatom: string option * Atom -> Unatom
    default this.Unatom_Unatom(operator, atom) =
        let operator' = operator
        let atom' = this.VisitAtom atom
        Unatom (operator', atom')
    
    abstract member VisitUnatom: Unatom -> Unatom
    default this.VisitUnatom(value) =
        match value with
        | Unatom (operator, atom) -> this.Unatom_Unatom(operator, atom)
