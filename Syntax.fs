module Syntax

open Parse
open Print

type LexprName =
    | [<Parse("IDENTIFIER")>] IdentifierN of string
    | [<Parse("OPERATOR")>] OperatorN of string

type Lexpr =
    | [<Parse("_"); 
        Parse("opt[] surr '(' ')' delim ',␠' _")>]
      Lexpr of LexprName * Lexpr list

type MonoType =
    | [<Parse("delim or '␠->␠' '␠→␠' _")>] MonoType of Lexpr list

type PolyType =
    | [<Parse("opt[] surr or 'forall␠' '∀␠' ⚠ ',␠' 1+ IDENTIFIER");
        Parse("delim '␠|␠' _")>] 
      PolyType of string list * MonoType list

type TypeDecl =
    | [<Parse("and 'type␠' ⚠ IDENTIFIER");
        Parse("opt[] surr '(' ')' ⚠ delim ',␠' IDENTIFIER");
        Parse("⚠ and '␠=␠' _␤")>]
      TypeDecl of string * string list * PolyType

type Postfix =
    | [<Parse("␑SUPERSCRIPT")>] SuperscriptPF of bigint

type Pattern =
    | [<Parse("IDENTIFIER");
        Parse("opt[] surr '(' ')' delim ',␠' _")>] 
      CtorPat of string * Pattern list
    | [<Parse("NAT")>]
      NatPat of bigint
    | [<Parse("STRING")>]
      StringPat of string

type LetDecl =
    | [<Parse("and 'let␠' ⚠ _");
        Parse("⚠ and '␠=␏' ␤_␤")>]
      LetDecl of Lexpr * Expr

and Atom =
    | [<Parse("NAT")>] NatA of bigint
    | [<Parse("STRING")>] StringA of string
    | [<Parse("OPERATOR")>] OperatorA of string
    | [<Parse("_")>] LambdaA of Lambda
    | [<Parse("surr '(' ')' ⚠ _")>] ParensA of Expr
    | [<Parse("IDENTIFIER")>] IdentifierA of string
    | [<Parse("_")>] CasesA of Cases
    | [<Parse("_")>] IfThenA of IfThen

and Case =
    | [<Parse("_");
        Parse("and ':␏' ␤_")>]
      Case of Pattern * Expr

and Cases =
    | [<Parse("and 'case␠' ⚠ _");
        Parse("⚠ and '␠of␏' 1+ ␤_");
        Parse("opt and '␤else␏' ⚠ ␤_")>]
      Cases of Expr * Case list * Expr option

and Expr =
    | [<Parse("0+ _␤");
        Parse("_");
        Parse("opt[] surr '(' ')' delim ',␠' _");
        Parse("0+ _")>]
      Expr of Prelude list * Atom * Expr list * Postfix list

and IfThen =
    | [<Parse("and 'if␏' ⚠ ␤_");
        Parse("⚠ and '␤then␠' _");
        Parse("⚠ and '␤else␠' _")>]
      IfThen of Expr * Expr * Expr

and Lambda =
    | [<Parse("surr '(' ')' _");
        Parse("and '␠=␠' ⚠ _")>]
      Lambda of Pattern * Expr

and Prelude =
    | [<Parse("_")>] TypeP of TypeDecl
    | [<Parse("_")>] LetP of LetDecl

and Command =
    | [<Parse("_")>] TypeCmd of TypeDecl
    | [<Parse("_")>] LetCmd of LetDecl
    | [<Parse("_")>] ExprCmd of Expr
