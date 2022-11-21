module Parser
// Generated code. Do not edit.

open Tokens
open Lexer
open Parse
open Syntax

let keywords = Set [
    "case";
    "elif";
    "else";
    "fn";
    "forall";
    "if";
    "let";
    "of";
    "then";
    "type";
]

let rec parseAtom tokens =
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
            let! f0 = parseLambda
            return LambdaA(f0)
        }
        return! parser {
            let! f0 = surround (literal "(") (literal ")") (checkpoint (parseExpr))
            return ParensA(f0)
        }
        return! parser {
            let! f0 = stringToken Identifier "Identifier"
            return IdentifierA(f0)
        }
        return! parser {
            let! f0 = parseCases
            return CasesA(f0)
        }
        return! parser {
            let! f0 = parseIfThen
            return IfThenA(f0)
        }
        return! parser {
            let! f0 = parseLetDecl
            return LetA(f0)
        }
        return! parser {
            let! f0 = parseTypeDecl
            return TypeA(f0)
        }
    }
    p tokens

and parseCase tokens =
    let p = parser {
        let! f0 = parsePattern
        let! f1 = andThen (literal ":") (parseExpr)
        return Case(f0, f1)
    }
    p tokens

and parseCases tokens =
    let p = parser {
        let! f0 = andThen (literal "case") (checkpoint (parseExpr))
        let! f1 = checkpoint (andThen (literal "of") (oneOrMore (parseCase)))
        let! f2 = option (andThen (literal "else") (checkpoint (parseExpr)))
        return Cases(f0, f1, f2)
    }
    p tokens

and parseElseIf tokens =
    let p = parser {
        let! f0 = andThen (literal "elif") (checkpoint (parseExpr))
        let! f1 = checkpoint (andThen (literal "then") (parseExpr))
        return ElseIf(f0, f1)
    }
    p tokens

and parseExpr tokens =
    let p = parser {
        let! f0 = parseAtom
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parseExpr)))
        let! f2 = zeroOrMore (parsePostfix)
        return Expr(f0, f1, f2)
    }
    p tokens

and parseIfThen tokens =
    let p = parser {
        let! f0 = andThen (literal "if") (checkpoint (parseExpr))
        let! f1 = checkpoint (andThen (literal "then") (parseExpr))
        let! f2 = zeroOrMore (parseElseIf)
        let! f3 = checkpoint (andThen (literal "else") (parseExpr))
        return IfThen(f0, f1, f2, f3)
    }
    p tokens

and parseLambda tokens =
    let p = parser {
        let! f0 = andThen (literal "fn") (surround (literal "(") (literal ")") (parseLexpr))
        let! f1 = andThen (literal "=") (checkpoint (parseExpr))
        return Lambda(f0, f1)
    }
    p tokens

and parseLetDecl tokens =
    let p = parser {
        let! f0 = andThen (literal "let") (checkpoint (parseLexpr))
        let! f1 = checkpoint (andThen (literal "=") (parseExpr))
        let! f2 = checkpoint (parseExpr)
        return LetDecl(f0, f1, f2)
    }
    p tokens

and parseLexpr tokens =
    let p = parser {
        let! f0 = parseLexprName
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parseLexpr)))
        return Lexpr(f0, f1)
    }
    p tokens

and parseLexprName tokens =
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

and parseMonoType tokens =
    let p = parser {
        let! f0 = delimited (orElse (literal "->") (literal "→")) (parseLexpr)
        return MonoType(f0)
    }
    p tokens

and parsePattern tokens =
    let p = parser {
        return! parser {
            let! f0 = stringToken Identifier "Identifier"
            let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parsePattern)))
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
        return! parser {
            let! f0 = boolToken Bool "Bool"
            return BoolPat(f0)
        }
    }
    p tokens

and parsePolyType tokens =
    let p = parser {
        let! f0 = optionlist (surround (orElse (literal "forall") (literal "∀")) (checkpoint (literal ",")) (oneOrMore (stringToken Identifier "Identifier")))
        let! f1 = delimited (literal "|") (parseMonoType)
        return PolyType(f0, f1)
    }
    p tokens

and parsePostfix tokens =
    let p = parser {
        let! f0 = bigintToken Superscript "Superscript"
        return SuperscriptPF(f0)
    }
    p tokens

and parseTypeDecl tokens =
    let p = parser {
        let! f0 = andThen (literal "type") (checkpoint (stringToken Identifier "Identifier"))
        let! f1 = optionlist (surround (literal "(") (literal ")") (checkpoint (delimited (literal ",") (stringToken Identifier "Identifier"))))
        let! f2 = checkpoint (andThen (literal "=") (parsePolyType))
        let! f3 = parseExpr
        return TypeDecl(f0, f1, f2, f3)
    }
    p tokens
