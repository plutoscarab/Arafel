using System.Diagnostics;
using System.Globalization;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;

internal sealed class Lexer
{
    static Dictionary<Rune, Func<Cursor, Token>> symbols = new Dictionary<Rune, Type>
    {
        { new Rune('('), typeof(LParenToken) },
        { new Rune(')'), typeof(RParenToken) },
        { new Rune('['), typeof(LBrackToken) },
        { new Rune(']'), typeof(RBrackToken) },
        { new Rune('{'), typeof(LBraceToken) },
        { new Rune('}'), typeof(RBraceToken) },
        { new Rune(','), typeof(CommaToken) },
    }.ToDictionary(e => e.Key, e =>
    {
        var ctor = e.Value.GetConstructor(
            BindingFlags.Public | BindingFlags.Instance, 
            null,
            new[] { typeof(Cursor) },
            null);
            
        if (ctor == null) throw new ArgumentNullException();
        var param = Expression.Parameter(typeof(Cursor), "Start");
        var newExpr = Expression.New(ctor, new[] {param});
        var expr = Expression.Convert(newExpr, typeof(Token));
        var l = Expression.Lambda(expr, false, new[] {param});
        var del = l.Compile();
        return (Func<Cursor, Token>)del;
    });

    static HashSet<string> keywords = new HashSet<string>{ "type", "case", "of", "let", "op" };
    const string super = "⁰¹²³⁴⁵⁶⁷⁸⁹";
    const string ops = "~!@#$%^&*-+=|\\:;<>.?/√∛∜";


        static IEnumerable<(Rune, int, Func<Rune, Cursor, Cursor, Token>?)> Make(Predicate<Rune> filter, int state, Func<Rune, Cursor, Cursor, Token>? fn = null)
        {
            for (var cp = 0; cp < 0x110000; cp++)
            {
                if (cp >= 0xD800 && cp < 0xE000)
                    continue;

                var rune = new Rune(cp);

                if (filter(rune))
                {
                    yield return (rune, state, fn);
                }
            }
        }

        static IEnumerable<(Rune, int, Func<Rune, Cursor, Cursor, Token>?)> MakeC(Predicate<char> filter, int state, Func<Rune, Cursor, Cursor, Token>? fn = null)
        {
            for (var cp = 0; cp < 0x10000; cp++)
            {
                if (cp >= 0xD800 && cp < 0xE000)
                    continue;

                var ch = (char)cp;

                if (filter(ch))
                {
                    yield return (new Rune(ch), state, fn);
                }
            }
        }

        static Dictionary<Rune, (int, Func<Rune, Cursor, Cursor, Token>?)>[] transitions = new IEnumerable<(Rune, int, Func<Rune, Cursor, Cursor, Token>?)>[] {
            // 0: start of line
            MakeC(c => c is '\r' or '\n', 0)
            .Concat(Make(Rune.IsWhiteSpace, 2)),

            // 1: comment
            MakeC(c => c == '\0', -1)
            .Concat(MakeC(c => c is '\r' or '\n', 0)),

            // 2: between tokens
            MakeC(c => c == '\0', -1)
            .Concat(MakeC(c => c is '\r' or '\n', 0))
            .Concat(Make(Rune.IsWhiteSpace, 2))
            .Concat(MakeC(c => c == '"', 3))
            .Concat(MakeC(c => c == '\'', 4))
            .Concat(MakeC(c => super.Contains(c), 5))
            .Concat(Make(Rune.IsDigit, 6))
            .Concat(Make(r => symbols.TryGetValue(r, out var _), 2, (r,_,cu)=>symbols[r](cu)))
            .Concat(Make(r => Rune.IsLetter(r) || r.Is("_"), 7))
            .Concat(MakeC(c => c == '.', 8))
            .Concat(Make(r => ops.Contains(r.ToString()) || Rune.GetUnicodeCategory(r) is UnicodeCategory.MathSymbol or UnicodeCategory.OtherSymbol, 14)),

            // 3: string literal
            MakeC(c => c == '"', 2, (_,st,cu)=> new StringToken(st.Next(), cu)),

            // 4: start of char literal
            MakeC(c => c != '\'', 9),

            // 5: superscript
            MakeC(c => super.Contains(c), 5),

            // 6: first digitgroups
            MakeC(c => c == '_', 10)
            .Concat(MakeC(c => c == '.', 11))
            .Concat(Make(Rune.IsDigit, 6)),

            // 7: identifier
            Make(r => r.Is("_") || Rune.IsLetter(r) || Rune.IsDigit(r), 7),

            // 8: // second character of operator starting with "."
            MakeC(c => c == '.', 2, (_, st, cu) => new OperatorToken(st, cu.Next()))
            .Concat(MakeC(c => ops.Contains(c), 14)),

            // 9: end of char literal
            MakeC(c => c == '\'', 2, (_, st, cu) => new CharToken(st.Next(), cu)),

            // 10: after _ in first digitGroups
            Make(Rune.IsDigit, 6),

            // 11: start of second digitGroups
            MakeC(c => c == '.', 2, (_, st, cu) => new NatListToken(st, cu))
            .Concat(Make(Rune.IsDigit, 12)),

            // 12: second digitGroups
            MakeC(c => c == '_', 13)
            .Concat(Make(Rune.IsDigit, 12)),

            // 13: after _ in second digitGroups
            Make(Rune.IsDigit, 12),

            // 14: operator
            Make(r => ops.Contains(r.ToString()) || Rune.GetUnicodeCategory(r) is UnicodeCategory.MathSymbol or UnicodeCategory.OtherSymbol, 14),
        }
        .Select(items => MakeDictionary(items))
        .ToArray();

    private static Dictionary<Rune, (int, Func<Rune, Cursor, Cursor, Token>?)> MakeDictionary(IEnumerable<(Rune, int, Func<Rune, Cursor, Cursor, Token>?)> items)
    {
        var dict = new Dictionary<Rune, (int, Func<Rune, Cursor, Cursor, Token>?)>();

        foreach (var item in items)
        {
            if (!dict.ContainsKey(item.Item1))
            {
                dict[item.Item1] = (item.Item2, item.Item3);
            }
        }

        return dict;
    }

    static (int, Func<Rune, Cursor, Cursor, Token>?)[] fallthrough = new [] {
        (1, (Func<Rune, Cursor, Cursor, Token>?)null),
        (1, null),
        (2, (_, _, cu) => new UnknownToken(cu, cu.Next())),
        (3, null),
        (2, (_, _, cu) => new UnknownToken(cu, cu.Next())),
        (-2, (_, st, cu) => new SuperToken(st, cu)),
        (-2, (_, st, cu) => new NatToken(st, cu)),
        (-2, (_, st, cu) => {
            var id = new IdToken(st, cu);
            if (keywords.Contains(id.Text))
                return new KeywordToken(st, cu);
            return id;
        }),
        (-2, (_, st, cu) => new OperatorToken(st, cu)),
        (-2, (_, st, cu) => new UnknownToken(st, cu)),
        (-2, (_, st, cu) => new UnknownToken(st, cu)),
        (-2, (_, st, cu) => new UnknownToken(st, cu)),
        (-2, (_, st, cu) => new DecimalToken(st, cu)),
        (-2, (_, st, cu) => new UnknownToken(st, cu)),
        (-2, (_, st, cu) => new OperatorToken(st, cu)),
    };

    public static IEnumerable<Token> Dfa(string text, Operators operators)
    {
        var cursor = new Cursor(text, new Context(operators));
        var state = 0;
        var start = cursor;
        var previous = cursor;

        while (state != -1)
        {
            var rune = cursor.Current;
            var next = cursor.Next();
            var s = rune.ToString();

stationary:
            if (state == 2) 
                start = cursor;

            Func<Rune, Cursor, Cursor, Token>? fn = null;

            if (!transitions[state].TryGetValue(rune, out var value))
                value = fallthrough[state];

            (state, fn) = value;

            if (fn is not null) 
            {
                var token = fn(rune, start, cursor);

                if (token is NatListToken nl)
                {
                    yield return new NatToken(nl.Start, previous);
                    yield return new OperatorToken(previous, nl.Next.Next());
                }
                else
                {
                    yield return token;
                }
            }

            if (state < -1)
            {
                state = -state;
                goto stationary;
            }

            previous = cursor;
            cursor = next;
        }
    }

    public static IEnumerable<Token> Tokenize(string text, Operators operators)
    {
        var runes = text.EnumerateRunes().ToArray();
        var cursor = new Cursor(runes, new Context(operators));
        var indented = false;

        while (cursor.More)
        {
            var rune = cursor.Current;

            if (rune.Is("\r") || rune.Is("\n"))
            {
                indented = false;
                cursor = cursor.Next();
                continue;
            }

            if (Rune.IsWhiteSpace(rune))
            {
                cursor = cursor.Next();
                indented = true;
                continue;
            }

            if (!indented)
            {
                var line = cursor.Line;
                do { cursor = cursor.Next(); } while (cursor.More && line == cursor.Line);
                continue;
            }

            var start = cursor;

            if (rune.Is("\""))
            {
                cursor = cursor.Next();

                while (cursor.More && cursor.Current.IsNot("\""))
                    cursor = cursor.Next();

                if (cursor.Current.Is("\""))
                {
                    yield return new StringToken(start.Next(), cursor);
                    cursor = cursor.Next();
                    continue;
                }

                cursor = start;
                yield return new UnknownToken(cursor, cursor);
                cursor = cursor.Next();
                continue;
            }

            if (rune.Is("'"))
            {
                var end = cursor.Next().Next();

                if (end.Current.Is("'"))
                {
                    yield return new CharToken(cursor.Next(), end);
                    cursor = end.Next();
                }
                else
                {
                    cursor = cursor.Next();
                    yield return new UnknownToken(start, start.Next());
                }

                continue;
            }

            while (super.Contains(cursor.Current.ToString()))
            {
                cursor = cursor.Next();
            }

            if (cursor > start)
            {
                yield return new SuperToken(start, cursor);
                continue;
            }

            var isDecimal = false;

            if (Rune.IsDigit(rune))
            {
                cursor = cursor.Next();

                while (Rune.IsDigit(cursor.Current))
                    cursor = cursor.Next();

                while (cursor.Current.Is("_"))
                {
                    if (!Rune.IsDigit(cursor.Next().Current))
                        break;

                    cursor = cursor.Next();

                    while (Rune.IsDigit(cursor.Current))
                        cursor = cursor.Next();
                }

                if (cursor.Current.Is(".") && Rune.IsDigit(cursor.Next().Current))
                {
                    isDecimal = true;
                    cursor = cursor.Next();

                    while (Rune.IsDigit(cursor.Current))
                        cursor = cursor.Next();

                    while (cursor.Current.Is("_"))
                    {
                        if (!Rune.IsDigit(cursor.Next().Current))
                            break;

                        cursor = cursor.Next();

                        while (Rune.IsDigit(cursor.Current))
                            cursor = cursor.Next();
                    }
                }
            }

            if (cursor > start)
            {
                if (isDecimal)
                    yield return new DecimalToken(start, cursor);
                else
                    yield return new NatToken(start, cursor);
                continue;
            }

            if (symbols.TryGetValue(cursor.Current, out var symbolConstructor))
            {
                yield return symbolConstructor(cursor);
                cursor = cursor.Next();
                continue;
            }

            if (Rune.IsLetter(rune) || rune.Is("_"))
            {
                cursor = cursor.Next();

                while (Rune.IsLetter(cursor.Current) || Rune.IsDigit(cursor.Current) || cursor.Current.Is("_"))
                {
                    cursor = cursor.Next();
                }

                var s = new Token(start, cursor).ToString();

                if (keywords.Contains(s))
                {
                    yield return new KeywordToken(start, cursor);
                    continue;
                }

                if (s == "false" || s == "true")
                {
                    yield return new BoolToken(start, cursor);
                    continue;
                }

                yield return new IdToken(start, cursor);
                continue;
            }

            while (ops.Contains(cursor.Current.ToString()) || char.GetUnicodeCategory(cursor.Current.ToString(), 0) is UnicodeCategory.MathSymbol or UnicodeCategory.OtherSymbol)
            {
                cursor = cursor.Next();

                if (cursor.Offset == start.Offset + 2 && start.Current.ToString() + start.Next().Current.ToString() == "..")
                    break;
            }

            if (cursor > start)
            {
                yield return new OperatorToken(start, cursor);
                continue;
            }

            yield return new UnknownToken(start, start.Next());
            cursor = cursor.Next();
        }
    }
}