module Arafel

open Tokens
open Lexer
open Parse
open Syntax

let rec postfix tokens =
    let p = parser {
        let! f0 = bigintToken Superscript "Superscript"
        return SuperscriptPF(f0)
    }
    p tokens

and ifthen tokens =
    let p = parser {
        let! f0 = andThen (literal "if") (checkpoint (expr))
        let! f1 = andThen (literal "then") (expr)
        let! f2 = andThen (literal "else") (expr)
        return IfThen(f0, f1, f2)
    }
    p tokens

and case tokens =
    let p = parser {
        let! f0 = pattern
        let! f1 = andThen (literal ":") (statement)
        return Case(f0, f1)
    }
    p tokens

and cases tokens =
    let p = parser {
        let! f0 = andThen (literal "case") (checkpoint (expr))
        let! f1 = andThen (literal "of") (oneOrMore (case))
        let! f2 = option (andThen (literal "else") (statement))
        return Cases(f0, f1, f2)
    }
    p tokens

and pattern tokens =
    let p = parser {
        return! parser {
            let! f0 = stringToken Id "Id"
            let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (pattern)))
            return CtorPat(f0, f1)
        }
        return! parser {
            let! f0 = bigintToken Nat "Nat"
            return NatPat(f0)
        }
        return! parser {
            let! f0 = stringToken String "String"
            return StringPat(f0)
        }
    }
    p tokens

and lambda tokens =
    let p = parser {
        let! f0 = surround (literal "(") (literal ")") (pattern)
        let! f1 = andThen (literal "=") (checkpoint (expr))
        return Lambda(f0, f1)
    }
    p tokens

and atom tokens =
    let p = parser {
        return! parser {
            let! f0 = bigintToken Nat "Nat"
            return NatA(f0)
        }
        return! parser {
            let! f0 = stringToken String "String"
            return StringA(f0)
        }
        return! parser {
            let! f0 = stringToken Operator "Operator"
            return OperatorA(f0)
        }
        return! parser {
            let! f0 = lambda
            return LambdaA(f0)
        }
        return! parser {
            let! f0 = surround (literal "(") (literal ")") (checkpoint (expr))
            return ParensA(f0)
        }
        return! parser {
            let! f0 = stringToken Id "Id"
            return IdentifierA(f0)
        }
        return! parser {
            let! f0 = cases
            return CasesA(f0)
        }
        return! parser {
            let! f0 = ifthen
            return IfThenA(f0)
        }
    }
    p tokens

and expr tokens =
    let p = parser {
        let! f0 = atom
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (expr)))
        let! f2 = zeroOrMore (postfix)
        return Expr(f0, f1, f2)
    }
    p tokens

and letdecl tokens =
    let p = parser {
        let! f0 = andThen (literal "let") (checkpoint (lexpr))
        let! f1 = andThen (literal "=") (statement)
        return LetDecl(f0, f1)
    }
    p tokens

and lexprname tokens =
    let p = parser {
        return! parser {
            let! f0 = stringToken Id "Id"
            return IdentifierN(f0)
        }
        return! parser {
            let! f0 = stringToken Operator "Operator"
            return OperatorN(f0)
        }
    }
    p tokens

and lexpr tokens =
    let p = parser {
        let! f0 = lexprname
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (lexpr)))
        return Lexpr(f0, f1)
    }
    p tokens

and monotype tokens =
    let p = parser {
        let! f0 = delimited (orElse (literal "->") (literal "→")) (lexpr)
        return MonoType(f0)
    }
    p tokens

and polytype tokens =
    let p = parser {
        let! f0 = optionlist (surround (orElse (literal "forall") (literal "∀")) (checkpoint (literal ",")) (oneOrMore (stringToken Id "Id")))
        let! f1 = delimited (literal "|") (monotype)
        return PolyType(f0, f1)
    }
    p tokens

and typedecl tokens =
    let p = parser {
        let! f0 = andThen (literal "type") (checkpoint (stringToken Id "Id"))
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (stringToken Id "Id")))
        let! f2 = andThen (literal "=") (polytype)
        return TypeDecl(f0, f1, f2)
    }
    p tokens

and prelude tokens =
    let p = parser {
        return! parser {
            let! f0 = typedecl
            return TypeP(f0)
        }
        return! parser {
            let! f0 = letdecl
            return LetP(f0)
        }
    }
    p tokens

and statement tokens =
    let p = parser {
        let! f0 = zeroOrMore (prelude)
        let! f1 = expr
        return Statement(f0, f1)
    }
    p tokens
