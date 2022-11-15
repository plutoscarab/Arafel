module Arafel

open Language
open Tokens
open Lexer
open Parse

let rec postfix tokens =
    let p = parser {
        let! f0 = bigintToken Superscript
        return SuperscriptPF(f0)
    }
    p tokens

and ifthen tokens =
    let p = parser {
        let! f0 = andThen (literal "if□") (expr)
        let! f1 = andThen (literal "□then□") (expr)
        let! f2 = andThen (literal "□else□") (expr)
        return IfThen(f0, f1, f2)
    }
    p tokens

and case tokens =
    let p = parser {
        let! f0 = pattern
        let! f1 = andThen (literal "□->□") (statement)
        return Case(f0, f1)
    }
    p tokens

and matches tokens =
    let p = parser {
        let! f0 = andThen (literal "case□") (expr)
        let! f1 = oneOrMore (case)
        let! f2 = option (andThen (literal "□else□") (expr))
        return Matches(f0, f1, f2)
    }
    p tokens

and pattern tokens =
    let p = parser {
        return! parser {
            let! f0 = stringToken Id
            let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",□") (pattern)))
            return CtorPat(f0, f1)
        }
        return! parser {
            let! f0 = bigintToken Nat
            return NatPat(f0)
        }
        return! parser {
            let! f0 = stringToken String
            return StringPat(f0)
        }
    }
    p tokens

and lambda tokens =
    let p = parser {
        let! f0 = surround (literal "(") (literal ")") (pattern)
        let! f1 = andThen (literal "□=□") (expr)
        return Lambda(f0, f1)
    }
    p tokens

and atom tokens =
    let p = parser {
        return! parser {
            let! f0 = bigintToken Nat
            return NatA(f0)
        }
        return! parser {
            let! f0 = stringToken String
            return StringA(f0)
        }
        return! parser {
            let! f0 = stringToken Operator
            return OperatorA(f0)
        }
        return! parser {
            let! f0 = lambda
            return LambdaA(f0)
        }
        return! parser {
            let! f0 = surround (literal "(") (literal ")") (expr)
            return ParensA(f0)
        }
        return! parser {
            let! f0 = stringToken Id
            return IdentifierA(f0)
        }
        return! parser {
            let! f0 = matches
            return MatchA(f0)
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
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",□") (expr)))
        let! f2 = zeroOrMore (postfix)
        return Expr(f0, f1, f2)
    }
    p tokens

and monotype tokens =
    let p = parser {
        let! f0 = delimited (orElse (literal "□->□") (literal "□→□")) (lexpr)
        return MonoType(f0)
    }
    p tokens

and polytype tokens =
    let p = parser {
        let! f0 = optionlist (surround (orElse (literal "forall□") (literal "∀□")) (literal ",□") (oneOrMore (stringToken Id)))
        let! f1 = delimited (literal "□|□") (monotype)
        return PolyType(f0, f1)
    }
    p tokens

and typedecl tokens =
    let p = parser {
        let! f0 = andThen (literal "◁type□") (stringToken Id)
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",□") (stringToken Id)))
        let! f2 = andThen (literal "□=□") (polytype)
        return TypeDecl(f0, f1, f2)
    }
    p tokens

and lexprname tokens =
    let p = parser {
        return! parser {
            let! f0 = stringToken Id
            return IdentifierN(f0)
        }
        return! parser {
            let! f0 = stringToken Operator
            return OperatorN(f0)
        }
    }
    p tokens

and lexpr tokens =
    let p = parser {
        let! f0 = lexprname
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",□") (lexpr)))
        return Lexpr(f0, f1)
    }
    p tokens

and letdecl tokens =
    let p = parser {
        let! f0 = andThen (literal "◁let□") (lexpr)
        let! f1 = andThen (literal "□=□") (statement)
        return LetDecl(f0, f1)
    }
    p tokens

and prelude tokens =
    let p = parser {
        return! parser {
            let! f0 = letdecl
            return LetP(f0)
        }
        return! parser {
            let! f0 = typedecl
            return TypeP(f0)
        }
        return! parser {
            let! f0 = stringToken Comment
            return CommentP(f0)
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
