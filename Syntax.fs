module Syntax

open Parse

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
        Parse("⚠ out '␤␤' opt and 'in' _")>]
      LetDecl of name: Lexpr * expr: Expr * inExpr: Expr option

and Term =
    | [<Parse("␠OPERATOR");
        Parse("␠_")>]
      Term of operator: string * atom: Unatom

and Expr =
    | [<Parse("_");
        Parse("0+ _")>]
      Expr of term: Unatom * terms: Term list
    | [<Parse("_")>] TypeE of typeDecl: TypeDecl
    | [<Parse("_")>] LetE of letDecl: LetDecl
    | [<Parse("_")>] CasesE of cases: Cases
    | [<Parse("_")>] IfThenE of ifthen: IfThen
    | [<Parse("and 'af' surr '(' ')' _");
        Parse("and '␠=␠' ⚠ _")>]
      LambdaE of name: Lexpr * expr: Expr
    | [<Parse("surr '{' '}' _")>] CurlyLambdaE of expr: Expr

and Range =
    | [<Parse("NAT");
        Parse("and '..' opt NAT");
        Parse("opt and '␠by␠' NAT")>]
      Range of first: bigint * last: bigint option * skip: bigint option

and IntSeq =
    | [<Parse("opt[] delim ',␠' _")>] IntSeq of ranges: Range list

and Unatom =
    | [<Parse("opt OPERATOR");
        Parse("_")>]
      Unatom of operator: string option * atom: Atom

and Atom =
    | [<Parse("_");
        Parse("surr '(' ')' delim ',␠' _")>]
      CallA of fn: Atom * args: Expr list
    | [<Parse("_");
        Parse("␑SUPERSCRIPT")>]
      ExponentA of atom: Atom * exponent: bigint
    | [<Parse("surr '[' ']' _")>] IntSeqA of intSeq: IntSeq
    | [<Parse("NAT")>] NatA of value: bigint
    | [<Parse("␅STRING␅")>] StringA of value: string
    | [<Parse("BOOL")>] BoolA of value: bool
    | [<Parse("surr '[' ']' OPERATOR")>] OperatorA of symbol: string
    | [<Parse("surr '(' ')' ⚠ _")>] ParensA of expr: Expr
    | [<Parse("IDENTIFIER")>] IdentifierA of name: string

and Case =
    | [<Parse("_");
        Parse("and ':␏' ␤_")>]
      Case of pattern: Pattern * expr: Expr

and Cases =
    | [<Parse("and 'case␠' ⚠ _");
        Parse("⚠ and '␠of␏' 1+ ␤_");
        Parse("opt and '␤else␏' ⚠ ␤_")>]
      Cases of expr: Expr * cases: Case list * otherwise: Expr option

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
