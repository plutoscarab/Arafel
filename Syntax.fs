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

type TypeDecl =
    | [<Parse("and 'type␠' ⚠ IDENTIFIER");
        Parse("opt[] surr '(' ')' ⚠ delim ',␠' IDENTIFIER");
        Parse("⚠ and '␠=␠' _␤")>]
      TypeDecl of name: string * parameters: string list * ptype: PolyType

type Postfix =
    | [<Parse("␑SUPERSCRIPT")>] SuperscriptPF of value: bigint

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

type LetDecl =
    | [<Parse("and 'let␠' ⚠ _");
        Parse("⚠ and '␠=␏' ␤_␤")>]
      LetDecl of name: Lexpr * expr: Expr

and Atom =
    | [<Parse("NAT")>] NatA of value: bigint
    | [<Parse("STRING")>] StringA of value: string
    | [<Parse("OPERATOR")>] OperatorA of symbol: string
    | [<Parse("_")>] LambdaA of lambda: Lambda
    | [<Parse("surr '(' ')' ⚠ _")>] ParensA of expr: Expr
    | [<Parse("IDENTIFIER")>] IdentifierA of name: string
    | [<Parse("_")>] CasesA of cases: Cases
    | [<Parse("_")>] IfThenA of ifthen: IfThen

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
    | [<Parse("0+ _␤");
        Parse("_");
        Parse("opt[] surr '(' ')' delim ',␠' _");
        Parse("0+ _")>]
      Expr of prelude: Prelude list * atom: Atom * args: Expr list * post: Postfix list

and IfThen =
    | [<Parse("and 'if␠' ⚠ _");
        Parse("⚠ and '␠then␠' _␏");
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

and Prelude =
    | [<Parse("_")>] TypeP of typeDecl: TypeDecl
    | [<Parse("_")>] LetP of letDecl: LetDecl

and Command =
    | [<Parse("_")>] TypeCmd of typeDecl: TypeDecl
    | [<Parse("_")>] LetCmd of letDecl: LetDecl
    | [<Parse("_")>] ExprCmd of expr: Expr
