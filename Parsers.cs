using System.Text;
    

internal delegate (Token? Result, Cursor Next) RuneParser(Cursor cursor);

internal sealed class RuneParsers
{
    public static RuneParser Any =
        cursor => (new Token(cursor), cursor.Next());

    public static RuneParser Expect(Predicate<Rune> predicate) =>
        cursor => predicate(cursor.Current) ? (new Token(cursor), cursor.Next()) : (null, cursor);

    public static RuneParser Expect<T>() =>
        Expect(rune => rune is T);

    public static RuneParser Expect(params Rune[] values) =>
        Expect(rune => values.Contains(rune));

    public static RuneParser OneOf(params RuneParser[] parsers) =>
        cursor =>
        {
            foreach (var parser in parsers)
            {
                var (result, next) = parser(cursor);
                if (result is not null) return (result, next);
            }

            return (null, cursor);
        };

    public static RuneParser Seq(params RuneParser[] parsers) =>
        cursor =>
        {
            var start = cursor;

            foreach (var parser in parsers)
            {
                var (result, next) = parser(cursor);
                if (result is null) return (null, cursor);
                cursor = next;
            }

            return (new Token(start, cursor), cursor);
        };

    public static RuneParser OneOrMore(RuneParser parser) =>
        cursor =>
        {
            var start = cursor;
            
            while (true)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;
                cursor = next;
            }
            
            if (cursor.Offset > start.Offset)
                return (new Token(start, cursor), cursor);
                
            return (null, start);
        };

    public static RuneParser Opt(RuneParser parser, Token defaultValue) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (defaultValue, cursor);
            return (result, next);
        };
}

internal sealed class TokenCursor
{
    public readonly IReadOnlyList<Token> Source;

    public readonly int Offset;

    public TokenCursor(IList<Token> source)
    {
        Source = (IReadOnlyList<Token>)source;
        Offset = 0;
    }

    private TokenCursor(TokenCursor cursor, int offset)
    {
        Source = cursor.Source;
        Offset = offset;
    }

    public bool More => Offset < Source.Count;

    public Token Current => More ? Source[Offset] : new Token();

    public TokenCursor Next() => More ? new TokenCursor(this, Offset + 1) : this;
}
    

internal delegate (TokenTree? Result, TokenCursor Next) TokenParser(TokenCursor cursor);

internal sealed class TokenParsers
{
    public static TokenParser Any =
        cursor => (new TokenTree(cursor), cursor.Next());

    public static TokenParser Expect(Predicate<Token> predicate) =>
        cursor => predicate(cursor.Current) ? (new TokenTree(cursor), cursor.Next()) : (null, cursor);

    public static TokenParser Expect<T>() =>
        Expect(token => token is T);

    public static TokenParser Expect(params Token[] values) =>
        Expect(token => values.Contains(token));

    public static TokenParser OneOf(params TokenParser[] parsers) =>
        cursor =>
        {
            foreach (var parser in parsers)
            {
                var (result, next) = parser(cursor);
                if (result is not null) return (result, next);
            }

            return (null, cursor);
        };

    public static TokenParser Seq(params TokenParser[] parsers) =>
        cursor =>
        {
            var start = cursor;

            foreach (var parser in parsers)
            {
                var (result, next) = parser(cursor);
                if (result is null) return (null, cursor);
                cursor = next;
            }

            return (new TokenTree(start, cursor), cursor);
        };

    public static TokenParser OneOrMore(TokenParser parser) =>
        cursor =>
        {
            var start = cursor;
            
            while (true)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;
                cursor = next;
            }
            
            if (cursor.Offset > start.Offset)
                return (new TokenTree(start, cursor), cursor);
                
            return (null, start);
        };

    public static TokenParser Opt(TokenParser parser, TokenTree defaultValue) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (defaultValue, cursor);
            return (result, next);
        };
}
