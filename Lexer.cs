using System.Linq.Expressions;
using System.Reflection;

internal sealed class Lexer
{
    static Dictionary<char, Func<Cursor, Token>> symbols = new Dictionary<char, Type>
    {
        { '(', typeof(OpenParensToken) },
        { ')', typeof(CloseParensToken) },
        { '[', typeof(OpenBracketToken) },
        { ']', typeof(CloseBracketToken) },
        { ',', typeof(SeparatorToken) },
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
            var ch = cursor.Current;

            if (ch == '\r' || ch == '\n')
            {
                indented = false;
                cursor = cursor.Next();
                continue;
            }

            if (char.IsWhiteSpace(ch))
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
            var previous = cursor;

            if (cursor.Current == '"')
            {
                cursor = cursor.Next();

                while (cursor.More && cursor.Current != '"')
                    cursor = cursor.Next();

                if (cursor.Current == '"')
                {
                    yield return new StringToken(start, cursor - start);
                    continue;
                }

                cursor = start;
                yield return new UnknownToken(cursor, 1);
                cursor = cursor.Next();
                continue;
            }

            if (cursor.Current == '\'')
            {
                var end = cursor.Next().Next();

                if (end.Current == '\'')
                {
                    cursor = end.Next();
                    yield return new CharToken(start, cursor - start);
                    continue;
                }
            }

            while (char.IsDigit(text, cursor.Offset) || cursor.Current == '_')
            {
                if (cursor.Current == '_' && previous.Current == '_')
                    break;

                previous = cursor;
                cursor = cursor.Next();
            }

            if (cursor > start)
            {
                if (start.Current != '_' && previous.Current != '_')
                {
                    yield return new NatToken(start, cursor - start);
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

            if (char.IsLetter(text, cursor.Offset) || cursor.Current == '_')
            {
                cursor = cursor.Next();

                while (char.IsLetter(text, cursor.Offset) || char.IsDigit(text, cursor.Offset))
                {
                    cursor = cursor.Next();
                }

                var s = text.Substring(start.Offset, cursor - start);

                if (keywords.Contains(s))
                {
                    yield return new KeywordToken(start, cursor - start);
                    continue;
                }

                yield return new IdentifierToken(start, cursor - start);
                continue;
            }

            while ("+-*/%<=>.|&^".Contains(cursor.Current))
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