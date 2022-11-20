module Visitor

open System
open Syntax

type Visitor() =
    
    abstract member Atom_NatA: bigint -> Atom
    default this.Atom_NatA(value) =
        let value' = value
        NatA (value')
    
    abstract member Atom_StringA: string -> Atom
    default this.Atom_StringA(value) =
        let value' = value
        StringA (value')
    
    abstract member Atom_OperatorA: string -> Atom
    default this.Atom_OperatorA(symbol) =
        let symbol' = symbol
        OperatorA (symbol')
    
    abstract member Atom_LambdaA: Lambda -> Atom
    default this.Atom_LambdaA(lambda) =
        let lambda' = this.VisitLambda lambda
        LambdaA (lambda')
    
    abstract member Atom_ParensA: Expr -> Atom
    default this.Atom_ParensA(expr) =
        let expr' = this.VisitExpr expr
        ParensA (expr')
    
    abstract member Atom_IdentifierA: string -> Atom
    default this.Atom_IdentifierA(name) =
        let name' = name
        IdentifierA (name')
    
    abstract member Atom_CasesA: Cases -> Atom
    default this.Atom_CasesA(cases) =
        let cases' = this.VisitCases cases
        CasesA (cases')
    
    abstract member Atom_IfThenA: IfThen -> Atom
    default this.Atom_IfThenA(ifthen) =
        let ifthen' = this.VisitIfThen ifthen
        IfThenA (ifthen')
    
    abstract member VisitAtom: Atom -> Atom
    default this.VisitAtom(value) =
        match value with
        | NatA (value) -> this.Atom_NatA(value)
        | StringA (value) -> this.Atom_StringA(value)
        | OperatorA (symbol) -> this.Atom_OperatorA(symbol)
        | LambdaA (lambda) -> this.Atom_LambdaA(lambda)
        | ParensA (expr) -> this.Atom_ParensA(expr)
        | IdentifierA (name) -> this.Atom_IdentifierA(name)
        | CasesA (cases) -> this.Atom_CasesA(cases)
        | IfThenA (ifthen) -> this.Atom_IfThenA(ifthen)
    
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
    
    abstract member Command_TypeCmd: TypeDecl -> Command
    default this.Command_TypeCmd(typeDecl) =
        let typeDecl' = this.VisitTypeDecl typeDecl
        TypeCmd (typeDecl')
    
    abstract member Command_LetCmd: LetDecl -> Command
    default this.Command_LetCmd(letDecl) =
        let letDecl' = this.VisitLetDecl letDecl
        LetCmd (letDecl')
    
    abstract member Command_ExprCmd: Expr -> Command
    default this.Command_ExprCmd(expr) =
        let expr' = this.VisitExpr expr
        ExprCmd (expr')
    
    abstract member VisitCommand: Command -> Command
    default this.VisitCommand(value) =
        match value with
        | TypeCmd (typeDecl) -> this.Command_TypeCmd(typeDecl)
        | LetCmd (letDecl) -> this.Command_LetCmd(letDecl)
        | ExprCmd (expr) -> this.Command_ExprCmd(expr)
    
    abstract member ElseIf_ElseIf: Expr * Expr -> ElseIf
    default this.ElseIf_ElseIf(condition, trueExpr) =
        let condition' = this.VisitExpr condition
        let trueExpr' = this.VisitExpr trueExpr
        ElseIf (condition', trueExpr')
    
    abstract member VisitElseIf: ElseIf -> ElseIf
    default this.VisitElseIf(value) =
        match value with
        | ElseIf (condition, trueExpr) -> this.ElseIf_ElseIf(condition, trueExpr)
    
    abstract member Expr_Expr: Prelude list * Atom * Expr list * Postfix list -> Expr
    default this.Expr_Expr(prelude, atom, args, post) =
        let prelude' = List.map this.VisitPrelude prelude
        let atom' = this.VisitAtom atom
        let args' = List.map this.VisitExpr args
        let post' = List.map this.VisitPostfix post
        Expr (prelude', atom', args', post')
    
    abstract member VisitExpr: Expr -> Expr
    default this.VisitExpr(value) =
        match value with
        | Expr (prelude, atom, args, post) -> this.Expr_Expr(prelude, atom, args, post)
    
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
    
    abstract member Lambda_Lambda: Pattern * Expr -> Lambda
    default this.Lambda_Lambda(pattern, expr) =
        let pattern' = this.VisitPattern pattern
        let expr' = this.VisitExpr expr
        Lambda (pattern', expr')
    
    abstract member VisitLambda: Lambda -> Lambda
    default this.VisitLambda(value) =
        match value with
        | Lambda (pattern, expr) -> this.Lambda_Lambda(pattern, expr)
    
    abstract member LetDecl_LetDecl: Lexpr * Expr -> LetDecl
    default this.LetDecl_LetDecl(name, expr) =
        let name' = this.VisitLexpr name
        let expr' = this.VisitExpr expr
        LetDecl (name', expr')
    
    abstract member VisitLetDecl: LetDecl -> LetDecl
    default this.VisitLetDecl(value) =
        match value with
        | LetDecl (name, expr) -> this.LetDecl_LetDecl(name, expr)
    
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
    
    abstract member VisitPattern: Pattern -> Pattern
    default this.VisitPattern(value) =
        match value with
        | CtorPat (ctor, args) -> this.Pattern_CtorPat(ctor, args)
        | NatPat (value) -> this.Pattern_NatPat(value)
        | StringPat (value) -> this.Pattern_StringPat(value)
    
    abstract member PolyType_PolyType: string list * MonoType list -> PolyType
    default this.PolyType_PolyType(foralls, cases) =
        let foralls' = foralls
        let cases' = List.map this.VisitMonoType cases
        PolyType (foralls', cases')
    
    abstract member VisitPolyType: PolyType -> PolyType
    default this.VisitPolyType(value) =
        match value with
        | PolyType (foralls, cases) -> this.PolyType_PolyType(foralls, cases)
    
    abstract member Postfix_SuperscriptPF: bigint -> Postfix
    default this.Postfix_SuperscriptPF(value) =
        let value' = value
        SuperscriptPF (value')
    
    abstract member VisitPostfix: Postfix -> Postfix
    default this.VisitPostfix(value) =
        match value with
        | SuperscriptPF (value) -> this.Postfix_SuperscriptPF(value)
    
    abstract member Prelude_TypeP: TypeDecl -> Prelude
    default this.Prelude_TypeP(typeDecl) =
        let typeDecl' = this.VisitTypeDecl typeDecl
        TypeP (typeDecl')
    
    abstract member Prelude_LetP: LetDecl -> Prelude
    default this.Prelude_LetP(letDecl) =
        let letDecl' = this.VisitLetDecl letDecl
        LetP (letDecl')
    
    abstract member VisitPrelude: Prelude -> Prelude
    default this.VisitPrelude(value) =
        match value with
        | TypeP (typeDecl) -> this.Prelude_TypeP(typeDecl)
        | LetP (letDecl) -> this.Prelude_LetP(letDecl)
    
    abstract member TypeDecl_TypeDecl: string * string list * PolyType -> TypeDecl
    default this.TypeDecl_TypeDecl(name, parameters, ptype) =
        let name' = name
        let parameters' = parameters
        let ptype' = this.VisitPolyType ptype
        TypeDecl (name', parameters', ptype')
    
    abstract member VisitTypeDecl: TypeDecl -> TypeDecl
    default this.VisitTypeDecl(value) =
        match value with
        | TypeDecl (name, parameters, ptype) -> this.TypeDecl_TypeDecl(name, parameters, ptype)