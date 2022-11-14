module Language

open Parse

type LexprName =
    | [<Parse("ID")>] IdentifierN of string
    | [<Parse("OPERATOR")>] OperatorN of string

type Lexpr =
    | [<Parse("_"); 
        Parse("opt[] surr '(' ')' delim ',' _")>]
      Lexpr of LexprName * Lexpr list

type MonoType =
    | [<Parse("delim or '->' '→' _")>] MonoType of Lexpr list

type PolyType =
    | [<Parse("opt[] surr or 'forall' '∀' ',' 1+ ID");
        Parse("delim '|' _")>] 
      PolyType of string list * MonoType list

type TypeDecl =
    | [<Parse("and 'type' ID");
        Parse("opt[] surr '(' ')' delim ',' ID");
        Parse("and '=' _")>]
      TypeDecl of string * string list * PolyType

type Postfix =
    | [<Parse("SUPERSCRIPT")>] SuperscriptPF of bigint

type Pattern =
    | [<Parse("ID");
        Parse("opt[] surr '(' ')' delim ',' _")>] 
      CtorPat of string * Pattern list
    | [<Parse("NAT")>]
      NatPat of bigint
    | [<Parse("STRING")>]
      StringPat of string

type LetDecl =
    | [<Parse("and 'let' _");
        Parse("and '=' _")>]
      LetDecl of Lexpr * Expr

and Atom =
    | [<Parse("NAT")>] NatA of bigint
    | [<Parse("STRING")>] StringA of string
    | [<Parse("OPERATOR")>] OperatorA of string
    | [<Parse("_")>] LambdaA of Lambda
    | [<Parse("surr '(' ')' _")>] ParensA of Expr
    | [<Parse("ID")>] IdentifierA of string
    | [<Parse("_")>] MatchA of Matches
    | [<Parse("_")>] IfThenA of IfThen

and Case =
    | [<Parse("_");
        Parse("and '->' _")>]
      Case of Pattern * Expr

and Matches =
    | [<Parse("and 'case' _");
        Parse("1+ _");
        Parse("opt and 'else' _")>]
      Matches of Expr * Case list * Expr option

and Expr =
    | [<Parse("0+ _");
        Parse("_");
        Parse("opt[] surr '(' ')' delim ',' _");
        Parse("0+ _")>]
      Expr of Prelude list * Atom * Expr list * Postfix list

and IfThen =
    | [<Parse("and 'if' _");
        Parse("and 'then' _");
        Parse("and 'else' _")>]
      IfThen of Expr * Expr * Expr

and Lambda =
    | [<Parse("surr '(' ')' _");
        Parse("and '=' _")>]
      Lambda of Pattern * Expr

and Prelude =
    | [<Parse("_")>] LetP of LetDecl
    | [<Parse("_")>] TypeP of TypeDecl
