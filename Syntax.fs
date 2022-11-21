module Syntax

open Parse
open Print

type LexprName =
    | [<Parse("IDENTIFIER")>] IdentifierN of name: string
    | [<Parse("OPERATOR")>] OperatorN of symbol: string

type Lexpr =
    | [<Parse("_"); 
        Parse("opt[] surr '(' ')' delim ',␠' _")>]
      Lexpr of name: LexprName * parameters: Lexpr list

type MonoType =
    | [<Parse("delim or '␠->␠' '␠→␠' _")>] MonoType of types: Lexpr list

type PolyType =
    | [<Parse("opt[] surr or 'forall␠' '∀␠' ⚠ ',␠' 1+ IDENTIFIER");
        Parse("delim '␠|␠' _")>] 
      PolyType of foralls: string list * cases: MonoType list

type Pattern =
    | [<Parse("IDENTIFIER");
        Parse("opt[] surr '(' ')' delim ',␠' _")>] 
      CtorPat of ctor: string * args: Pattern list
    | [<Parse("NAT")>]
      NatPat of value: bigint
    | [<Parse("STRING")>]
      StringPat of value: string
    | [<Parse("BOOL")>]
      BoolPat of value: bool

type TypeDecl =
    | [<Parse("and 'type␠' ⚠ IDENTIFIER");
        Parse("opt[] surr '(' ')' ⚠ delim ',␠' IDENTIFIER");
        Parse("⚠ and '␠=␠' _␤");
        Parse("␤_")>]
      TypeDecl of name: string * parameters: string list * ptype: PolyType * inExpr: Expr

and LetDecl =
    | [<Parse("and 'let␠' ⚠ _");
        Parse("⚠ and '␠=␏' ␤_␎");
        Parse("⚠ out '␤␤' _")>]
      LetDecl of name: Lexpr * expr: Expr * inExpr: Expr

and Exponent =
    | [<Parse("_");
        Parse("␑SUPERSCRIPT")>] 
      Exponent of expr: Expr * exponent: bigint

and Atom =
    | [<Parse("NAT")>] NatA of value: bigint
    | [<Parse("STRING")>] StringA of value: string
    | [<Parse("OPERATOR")>] OperatorA of symbol: string
    | [<Parse("_")>] LambdaA of lambda: Lambda
    | [<Parse("surr '(' ')' ⚠ _")>] ParensA of expr: Expr
    | [<Parse("IDENTIFIER")>] IdentifierA of name: string
    | [<Parse("_")>] CasesA of cases: Cases
    | [<Parse("_")>] IfThenA of ifthen: IfThen
    | [<Parse("_")>] LetA of letDecl: LetDecl
    | [<Parse("_")>] TypeA of typeDecl: TypeDecl

and Case =
    | [<Parse("_");
        Parse("and ':␏' ␤_")>]
      Case of pattern: Pattern * expr: Expr

and Cases =
    | [<Parse("and 'case␠' ⚠ _");
        Parse("⚠ and '␠of␏' 1+ ␤_");
        Parse("opt and '␤else␏' ⚠ ␤_")>]
      Cases of expr: Expr * cases: Case list * otherwise: Expr option

and Expr =
    | [<Parse("_");
        Parse("opt[] surr '(' ')' delim ',␠' _")>]
      Expr of atom: Atom * args: Expr list

and IfThen =
    | [<Parse("and 'if␠' ⚠ _");
        Parse("⚠ and '␠then␠' _");
        Parse("0+ _");
        Parse("⚠ and '␤else␠' _")>]
      IfThen of condition: Expr * trueExpr: Expr * elseifs: ElseIf list * falseExpr: Expr

and ElseIf =
    | [<Parse("and '␤elif␠' ⚠ _");
        Parse("⚠ and '␠then␠' _")>]
      ElseIf of condition: Expr * trueExpr: Expr

and Lambda =
    | [<Parse("and 'fn' surr '(' ')' _");
        Parse("and '␠=␠' ⚠ _")>]
      Lambda of name: Lexpr * expr: Expr
