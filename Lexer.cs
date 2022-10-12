using System.Linq.Expressions;
using System.Reflection;
using System.Text;

internal sealed class Lexer
{
    static Dictionary<Rune, Func<Cursor, Token>> symbols = new Dictionary<Rune, Type>
    {
        { new Rune('('), typeof(LParenToken) },
        { new Rune(')'), typeof(RParenToken) },
        { new Rune('['), typeof(LBrackToken) },
        { new Rune(']'), typeof(RBrackToken) },
        { new Rune(','), typeof(CommaToken) },
    }.ToDictionary(e => e.Key, e =>
    {
        var ctor = e.Value.GetConstructor(BindingFlags.Public | BindingFlags.Instance, new[] { typeof(Cursor) });
        if (ctor == null) throw new ArgumentNullException();
        var param = Expression.Parameter(typeof(Cursor), "Start");
        var newExpr = Expression.New(ctor, new[] {param});
        var expr = Expression.Convert(newExpr, typeof(Token));
        var l = Expression.Lambda(expr, false, new[] {param});
        var del = l.Compile();
        return (Func<Cursor, Token>)del;
    });

    static HashSet<string> keywords = new HashSet<string>{ "type", "case", "of" };

    public static IEnumerable<Token> Tokenize(string text)
    {
        var cursor = new Cursor(text);
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
                    yield return new StringToken(start, cursor - start);
                    continue;
                }

                cursor = start;
                yield return new UnknownToken(cursor, 1);
                cursor = cursor.Next();
                continue;
            }

            if (rune.Is("'"))
            {
                var end = cursor.Next().Next();

                if (end.Current.Is("'"))
                {
                    cursor = end.Next();
                    yield return new CharToken(start, cursor - start);
                }
                else
                {
                    cursor = cursor.Next();
                    yield return new UnknownToken(start, 1);
                }

                continue;
            }

            while ("⁰¹²³⁴⁵⁶⁷⁸⁹".Contains(cursor.Current.ToString()))
            {
                cursor = cursor.Next();
            }

            if (cursor > start)
            {
                yield return new SuperToken(start, cursor - start);
                continue;
            }

            var previous = cursor;

            if (Rune.IsDigit(rune))
            {
                while (Rune.IsDigit(cursor.Current) || cursor.Current.Is("_"))
                {
                    if (cursor.Current.Is("_") && previous.Current.Is("_"))
                        break;

                    previous = cursor;
                    cursor = cursor.Next();
                }
            }

            if (cursor > start)
            {
                if (previous.Current.IsNot("_"))
                {
                    if (cursor.Current.Is("."))
                    {
                        cursor = cursor.Next();

                        while (Rune.IsDigit(cursor.Current))
                        {
                            cursor = cursor.Next();
                        }

                        yield return new DecimalToken(start, cursor - start);
                        continue;
                    }

                    var s = text.Substring(start.Offset, cursor - start);
                    yield return new NatToken(start, cursor - start, NatToken.UnicodeParse(s));
                    continue;
                }

                cursor = start;
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

                while (Rune.IsLetter(cursor.Current) || Rune.IsDigit(cursor.Current))
                {
                    cursor = cursor.Next();
                }

                var s = text.Substring(start.Offset, cursor - start);

                if (keywords.Contains(s))
                {
                    yield return new KeywordToken(start, cursor - start);
                    continue;
                }

                if (s == "false" || s == "true")
                {
                    yield return new BoolToken(start, cursor - start);
                    continue;
                }

                yield return new IdToken(start, cursor - start);
                continue;
            }

            while ("+-*/%<=>.|&^".Contains(cursor.Current.ToString()))
            {
                cursor = cursor.Next();
            }

            if (cursor > start)
            {
                yield return new OperatorToken(start, cursor - start);
                continue;
            }

            yield return new UnknownToken(start, 1);
            cursor = cursor.Next();
        }
    }
}