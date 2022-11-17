module Arafel

open Tokens
open Lexer
open Parse
open Syntax

let keywords = Set [ "case"; "else"; "forall"; "if"; "let"; "of"; "then"; "type" ]

let rec atom tokens =
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
            let! f0 = stringToken Identifier "Identifier"
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

and case tokens =
    let p = parser {
        let! f0 = pattern
        let! f1 = andThen (literal ":") (expr)
        return Case(f0, f1)
    }
    p tokens

and cases tokens =
    let p = parser {
        let! f0 = andThen (literal "case") (checkpoint (expr))
        let! f1 = checkpoint (andThen (literal "of") (oneOrMore (case)))
        let! f2 = option (andThen (literal "else") (checkpoint (expr)))
        return Cases(f0, f1, f2)
    }
    p tokens

and command tokens =
    let p = parser {
        return! parser {
            let! f0 = typedecl
            return TypeCmd(f0)
        }
        return! parser {
            let! f0 = letdecl
            return LetCmd(f0)
        }
        return! parser {
            let! f0 = expr
            return ExprCmd(f0)
        }
    }
    p tokens

and expr tokens =
    let p = parser {
        let! f0 = zeroOrMore (prelude)
        let! f1 = atom
        let! f2 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (expr)))
        let! f3 = zeroOrMore (postfix)
        return Expr(f0, f1, f2, f3)
    }
    p tokens

and ifthen tokens =
    let p = parser {
        let! f0 = andThen (literal "if") (checkpoint (expr))
        let! f1 = checkpoint (andThen (literal "then") (expr))
        let! f2 = checkpoint (andThen (literal "else") (expr))
        return IfThen(f0, f1, f2)
    }
    p tokens

and lambda tokens =
    let p = parser {
        let! f0 = surround (literal "(") (literal ")") (pattern)
        let! f1 = andThen (literal "=") (checkpoint (expr))
        return Lambda(f0, f1)
    }
    p tokens

and letdecl tokens =
    let p = parser {
        let! f0 = andThen (literal "let") (checkpoint (lexpr))
        let! f1 = checkpoint (andThen (literal "=") (expr))
        return LetDecl(f0, f1)
    }
    p tokens

and lexpr tokens =
    let p = parser {
        let! f0 = lexprname
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (lexpr)))
        return Lexpr(f0, f1)
    }
    p tokens

and lexprname tokens =
    let p = parser {
        return! parser {
            let! f0 = stringToken Identifier "Identifier"
            return IdentifierN(f0)
        }
        return! parser {
            let! f0 = stringToken Operator "Operator"
            return OperatorN(f0)
        }
    }
    p tokens

and monotype tokens =
    let p = parser {
        let! f0 = delimited (orElse (literal "->") (literal "→")) (lexpr)
        return MonoType(f0)
    }
    p tokens

and pattern tokens =
    let p = parser {
        return! parser {
            let! f0 = stringToken Identifier "Identifier"
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

and polytype tokens =
    let p = parser {
        let! f0 = optionlist (surround (orElse (literal "forall") (literal "∀")) (checkpoint (literal ",")) (oneOrMore (stringToken Identifier "Identifier")))
        let! f1 = delimited (literal "|") (monotype)
        return PolyType(f0, f1)
    }
    p tokens

and postfix tokens =
    let p = parser {
        let! f0 = bigintToken Superscript "Superscript"
        return SuperscriptPF(f0)
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

and typedecl tokens =
    let p = parser {
        let! f0 = andThen (literal "type") (checkpoint (stringToken Identifier "Identifier"))
        let! f1 = optionlist (surround (literal "(") (literal ")") (checkpoint (delimited (literal ",") (stringToken Identifier "Identifier"))))
        let! f2 = checkpoint (andThen (literal "=") (polytype))
        return TypeDecl(f0, f1, f2)
    }
    p tokens
