module Language
// Generated code. Do not edit.

type Assign = Assign of Lexpr * Expr
and Atom = NatA of bigint | StrA of string | OpA of string | LambdaA of Lambda | ParensA of Expr | IdA of string | CasesA of Cases | IfThenA of IfThen
and Case = Case of Pattern * Expr
and Cases = Cases of Expr * Case list * Expr option
and Expr = Expr of Prelude list * Atom * Expr list * Postfix list
and IfThen = IfThen of Expr * Expr * Expr
and Lambda = Lambda of Pattern * Expr
and Lexpr = Lexpr of LexprName * Lexpr list
and LexprName = IdN of string | OpN of string
and MonoType = MonoType of Lexpr list
and Pattern = CtorPat of string * Pattern list | NatPat of bigint | StrPat of string
and Postfix = Postfix of string
and Prelude = AssignP of Assign | TypeDeclP of TypeDecl
and TypeDecl = TypeDecl of string * string list * Tyƿe
and Tyƿe = Tyƿe of string list * MonoType list
