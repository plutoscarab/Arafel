
using System.Collections.Immutable;
using System.Diagnostics;

internal delegate (TokenTree Result, Tursor Next)? Parser(Tursor tursor);

internal static class Arafel
{
    static Parser AnyToken =>
        tursor =>
            (new TokenTree(TreeKind.Token, tursor), tursor.Next());

    static Parser From<TToken>() =>
        tursor =>
            tursor.Current is TToken ? 
                (new TokenTree(TreeKind.Token, tursor), tursor.Next()) : null;

    static Parser From<TToken>(params string[] matches) =>
        tursor =>
            tursor.Current is TToken && matches.Contains(tursor.Current.Text) ? 
                (new TokenTree(TreeKind.Token, tursor), tursor.Next()) : null;

    static Parser Delay(Func<Parser> parser) =>
        tursor =>
            parser()(tursor);

    static Parser Sequence(TreeKind kind, params Parser[] parsers) =>
        tursor =>
        {
            var list = new List<(TreeLabel, TokenTree)>();

            foreach (var parser in parsers)
            {
                var result = parser(tursor);
                if (result == null) return result;

                if (result.Value.Result.Label != TreeLabel.Ignore)
                    list.Add((result.Value.Result.Label, result.Value.Result));

                tursor = result.Value.Next;
            }

            return (new TokenTree(kind, list.ToArray()), tursor);
        };

    static Parser OneOf(params Parser[] parsers) =>
        tursor =>
        {
            foreach (var parser in parsers)
            {
                var result = parser(tursor);
                if (result != null) return result;
            }

            return null;
        };

    static Parser Association_(Parser parser, params string[] operators) =>
        OneOf(
            Sequence(TreeKind.Association_, 
                parser.Label(TreeLabel.First),
                Sequence(TreeKind.OpAndOperand,
                    From<OperatorToken>(operators).Kind(TreeKind.Operator).Label(TreeLabel.Operator),
                    parser.Label(TreeLabel.Operand)
                    ).OneOrMore(TreeKind.Items).Label(TreeLabel.Rest)),
            parser);

    static Parser Association(Parser parser, params string[] operators) =>
        tursor =>
        {
            var result = Association_(parser, operators)(tursor);
            if (result == null) return null;
            var list = new List<TokenTree>();

            if (result.Value.Result.Kind == TreeKind.Association_)
            {
                list.Add(result.Value.Result.Parts[TreeLabel.First]);

                list.AddRange(result.Value.Result.Parts[TreeLabel.Rest].Parts
                    .Select(p => (-(int)p.Key, p.Value))
                    .OrderBy(p => p.Item1)
                    .SelectMany(p => new[] { 
                        p.Value.Parts[TreeLabel.Operator],
                        p.Value.Parts[TreeLabel.Operand]}));
            }
            else
            {
                list.Add(result.Value.Result);
            }

            if (list.Count > 1)
                return (new TokenTree(TreeKind.Association, list), result.Value.Next);
        
            return (list[0], result.Value.Next);
        };
                
    static Parser ListOf(Parser parser, Parser? delimiter = null) =>
        tursor =>
        {
            delimiter ??= From<CommaToken>();

            var assoc = Sequence(TreeKind.Association, 
                parser.Label(TreeLabel.First),
                Sequence(TreeKind.Item,
                    delimiter,
                    parser.Label(TreeLabel.Item)
                ).ZeroOrMore(TreeKind.Items).Label(TreeLabel.Rest));

            var result = assoc(tursor);
            if (result == null) return null;
            var list = new List<TokenTree>();

            if (result.Value.Result.Kind == TreeKind.Association)
            {
                list.Add(result.Value.Result.Parts[TreeLabel.First]);
                list.AddRange(result.Value.Result.Parts[TreeLabel.Rest].Parts
                    .Select(p => (-(int)p.Key, p.Value))
                    .OrderBy(p => p.Item1)
                    .Select(p => p.Value.Parts[TreeLabel.Item]));
            }
            else
            {
                list.Add(result.Value.Result);
            }

            return (new TokenTree(TreeKind.Items, list), result.Value.Next);
        };

    static Parser Operator(string symbol) =>
        From<OperatorToken>(symbol);

    static Parser Keyword(string keyword) =>
        From<KeywordToken>(keyword);

    public static IEnumerable<TokenTree> Parse(IList<Token> tokens)
    {
        var delayedExpr = From<Token>();
        var expr = Delay(() => delayedExpr);
        var open = From<LParenToken>();
        var close = From<RParenToken>();

        var identifier = From<IdToken>()
            .Kind(TreeKind.Identifier)
            .Label(TreeLabel.Identifier);
        
        var nat = From<NatToken>().Kind(TreeKind.Nat);
        var chr = From<CharToken>().Kind(TreeKind.Char);
        var str = From<StringToken>().Kind(TreeKind.String);

        var parens = Sequence(TreeKind.Parens,
            open,
            expr.Label(TreeLabel.Operand), 
            close);

        var call = Sequence(TreeKind.Call,
            identifier,
            open,
            ListOf(expr).Label(TreeLabel.Arguments),
            close);

        var emptyList = Sequence(TreeKind.List,
            From<LBrackToken>(),
            From<RBrackToken>());

        var list = OneOf(Sequence(TreeKind.List,
            From<LBrackToken>(),
            ListOf(expr).Label(TreeLabel.Values),
            (Sequence(TreeKind.Etc, Operator(".."), expr.Label(TreeLabel.Final).Optional()))
                .Label(TreeLabel.Etc).Optional(),
            From<RBrackToken>()), emptyList);

        var constructorPattern =
            Sequence(TreeKind.ConstructorPattern,
                identifier.Log("ctor id"),
                open,
                ListOf(identifier.Log("ctor arg")).Label(TreeLabel.Parameters),
                close);

        var matchCase = 
            Sequence(TreeKind.Pattern,
                constructorPattern.Label(TreeLabel.Pattern).Log("pattern"),
                Operator("->").Log("->"),
                expr.Label(TreeLabel.Expression).Log("result"));

        var match = 
            Sequence(TreeKind.Case,
                Keyword("case"),
                expr.Label(TreeLabel.Expression).Log("case value"),
                Keyword("of"),
                matchCase.OneOrMore(TreeKind.Patterns).Label(TreeLabel.Patterns));

        var atom = OneOf(nat, chr, str, call, identifier, parens, list, match);

        var primaryDelayed = From<Token>();
        var primary = Delay(() => primaryDelayed);

        var unary = Sequence(TreeKind.Unary,
            From<OperatorToken>("+", "-").OneOrMore(TreeKind.Operator).Label(TreeLabel.Operator),
            primary.Label(TreeLabel.Operand));

        primaryDelayed = OneOf(atom, unary);
        var power = Association(primary, "**");
        var term = Association(power, "*", "/", "//", "%");
        var sum = Association(term, "+", "-");
        delayedExpr = sum;

        var function = Sequence(TreeKind.Function,
            identifier,
            open,
            ListOf(identifier).Label(TreeLabel.Parameters),
            close,
            Operator("="),
            expr.Label(TreeLabel.Operand));

        var assignment = Sequence(TreeKind.Assignment,
            identifier,
            Operator("="),
            expr.Label(TreeLabel.Expression));

        var unionElemDelayed = From<Token>();
        var unionElem = Delay(() => unionElemDelayed);

        unionElemDelayed = Sequence(TreeKind.Item,
            identifier,
            Sequence(TreeKind.Items,
                open,
                ListOf(unionElem).Label(TreeLabel.Arguments),
                close).Unwrap(TreeLabel.Arguments).Optional());

        var union = Sequence(TreeKind.Union,
            Keyword("type"),
            identifier,
            Sequence(TreeKind.Items,
                open,
                ListOf(identifier).Label(TreeLabel.Parameters),
                close).Unwrap(TreeLabel.Parameters).Optional(),
            Operator("="),
            ListOf(unionElem, Operator("|")).Label(TreeLabel.Constructors));

        var typeDef = union;
        var parser = OneOf(assignment, typeDef, function, expr);
        var tursor = new Tursor((IReadOnlyList<Token>)tokens);

        while (tursor.More)
        {
            var result = parser(tursor);

            if (result == null)
            {
                yield return new TokenTree(TreeKind.SyntaxError, tursor);
                tursor = tursor.Next();
                continue;
            }

            yield return result.Value.Result;
            tursor = result.Value.Next;
        }
    }
}

static class ParserExtensions
{
    public static Parser Kind(this Parser parser, TreeKind kind) =>
        tursor =>
        {
            var result = parser(tursor);
            if (result == null) return null;
            var value = result.Value;
            return (new TokenTree(kind, value.Result.Start, value.Result.Next, value.Result.Parts), value.Next);
        };

    public static Parser OneOrMore(this Parser parser, TreeKind kind) =>
        tursor =>
        {
            var result = parser.ZeroOrMore(kind)(tursor);
            if (result == null) return null;
            var value = result.Value;
            if (!value.Result.Parts.Any()) return null;
            return (value.Result, value.Next);
        };

    public static Parser ZeroOrMore(this Parser parser, TreeKind kind) =>
        tursor =>
        {
            var list = new List<TokenTree>();

            while (true)
            {
                var result = parser(tursor);
                if (result == null) break;
                var value = result.Value;
                list.Add(value.Result);
                tursor = value.Next;
            }

            return (new TokenTree(kind, list), tursor);
        };

    public static Parser Label(this Parser parser, TreeLabel label) =>
        tursor =>
        {
            var result = parser(tursor);
            if (result == null) return null;
            return (result.Value.Result.Tag(label), result.Value.Next);
        };

    public static Parser Optional(this Parser parser) =>
        tursor =>
        {
            var result = parser(tursor);

            if (result == null)
                return (new TokenTree(TreeKind.Nop, tursor, tursor), tursor);

            return (result.Value.Result, result.Value.Next);
        };

    public static Parser Break(this Parser parser) =>
        tursor =>
        {
            var result = parser(tursor);
            if (result == null) return null;
            Debugger.Break();
            return result;
        };

    public static Parser Unwrap(this Parser parser, TreeLabel label) =>
        tursor =>
        {
            var result = parser(tursor);
            if (result == null) return null;
            
            if (result.Value.Result.Parts.TryGetValue(label, out var value))
                return (value.Tag(label), result.Value.Next);

            return result;
        };

    public static Parser Log(this Parser parser, string message) =>
        tursor =>
        {
            var result = parser(tursor);
#if LOGGING
            if (result == null)
            {
                Console.WriteLine($"{message} failed");
                return null;
            }
            Console.WriteLine($"{message} succeeded");
#endif
            return result;
        };
}