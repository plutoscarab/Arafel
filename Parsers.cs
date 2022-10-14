using System.Text;
using System.Threading;

internal sealed partial record Cursor(IReadOnlyList<Rune> Source, int Offset)
{
    public Cursor(IList<Rune> source)
    : this((IReadOnlyList<Rune>)source, 0)
    { }

    private Cursor(Cursor cursor, int offset)
    : this(cursor.Source, offset)
    { }

    public bool More => Offset < Source.Count;

    public Rune Current => More ? Source[Offset] : new Rune();

    public static bool operator >(Cursor a, Cursor b) => a.Offset > b.Offset;
    
    public static bool operator <(Cursor a, Cursor b) => a.Offset < b.Offset;

    public static int operator -(Cursor a, Cursor b) => a.Offset - b.Offset;
}

internal partial record Token(Cursor Start, Cursor Next)
{
    public readonly List<Token> Children = new();

    public string Production = string.Empty;

    public Token(Cursor start) : this(start, start.Next()) { }

    public Token(Cursor start, Cursor next, IEnumerable<Token> children) 
    : this(start, next) 
    { 
        Children = children.ToList(); 
    }

    public IEnumerable<Rune> Runes()
    {
        var t = Start;

        while (t.More && t.Offset < Next.Offset)
        {
            yield return t.Current;
            t = t.Next();
        }
    }

    public void Dump(TextWriter writer)
    {
        writer.WriteLine($"Line {Start.Line} Col {Start.Col}");
        Dump(writer, string.Empty);
    }

    private void Dump(TextWriter writer, string indent, bool last = true)
    {
        var branch = last ? "└" : "├";
        var prod = Production;
        if (prod != "") prod += ": ";
        var token = Children.Any() ? "" : string.Join("", Runes().Select(t => t.ToString()));
        writer.WriteLine($"{indent}{branch}─{prod}{token}");
        indent += last ? "  " : "│ ";
        
        foreach (var (child, i) in Children.Select((c, i) => (c, i)))
        {
            child.Dump(writer, indent, i == Children.Count - 1);
        }
    }
}

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
            List<Token> list = new();

            foreach (var parser in parsers)
            {
                var (result, next) = parser(cursor);
                if (result is null) return (null, cursor);

                if (result.Next.Offset > result.Start.Offset) 
                    if (result.Children.Any() && string.IsNullOrEmpty(result.Production))
                        list.AddRange(result.Children);
                    else
                        list.Add(result);

                cursor = next;
            }

            if (list.Count == 1)
                return (list[0], cursor);

            return (new Token(start, cursor, list), cursor);
        };

    public static RuneParser OneOrMore(RuneParser parser) =>
        cursor =>
        {
            var start = cursor;
            List<Token> list = new();
            
            while (true)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;

                if (result.Next.Offset > result.Start.Offset) 
                    if (result.Children.Any() && string.IsNullOrEmpty(result.Production))
                        list.AddRange(result.Children);
                    else
                        list.Add(result);

                cursor = next;
            }
            
            if (cursor.Offset > start.Offset)
            {
                if (list.Count == 1)
                    return (list[0], cursor);
                    
                return (new Token(start, cursor, list), cursor);
            }
                
            return (null, start);
        };

    public static RuneParser ZeroOrMore(RuneParser parser) =>
        cursor =>
        {
            var start = cursor;
            List<Token> list = new();
            
            while (true)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;

                if (result.Next.Offset > result.Start.Offset) 
                    if (result.Children.Any() && string.IsNullOrEmpty(result.Production))
                        list.AddRange(result.Children);
                    else
                        list.Add(result);

                cursor = next;
            }
            
            if (cursor.Offset > start.Offset)
            {
                if (list.Count == 1)
                    return (list[0], cursor);
                    
                return (new Token(start, cursor, list), cursor);
            }
                
            return (new Token(start, start), start);
        };

    public static RuneParser Opt(RuneParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (new Token(cursor, cursor), cursor);
            return (result, next);
        };

    public static RuneParser Delay(Func<RuneParser> delayed) =>
        cursor => delayed()(cursor);

    static System.CodeDom.Compiler.IndentedTextWriter writer = new (Console.Out);

    public static RuneParser Prod(string production, RuneParser parser) =>
        cursor => 
        {
            //writer.WriteLine($"{production} at {cursor.Offset}");
            //writer.Indent++;
            var (result, next) = parser(cursor);
            //var message = result is null ? $"failed" : $"succeeded";
            //writer.WriteLine($"{production} {message}");
            //writer.Indent--;
            if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = production;
            return (result, next);
        };

    public static RuneParser Ignore(RuneParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (null, cursor);
            result = new Token(next, next, result.Children) { Production = result.Production };
            return (result, next);
        };

    public static RuneParser Merge(RuneParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (null, cursor);
            if (!result.Children.Any()) return (result, next);
            result = new Token(result.Children[0].Start, result.Children[^1].Next) { Production = result.Production };
            return (result, next);
        };
}

internal sealed partial record TokenCursor(IReadOnlyList<Token> Source, int Offset)
{
    public TokenCursor(IList<Token> source)
    : this((IReadOnlyList<Token>)source, 0)
    { }

    private TokenCursor(TokenCursor cursor, int offset)
    : this(cursor.Source, offset)
    { }

    public bool More => Offset < Source.Count;

    public Token Current => More ? Source[Offset] : new Token(Cursor.Empty, Cursor.Empty);

    public static bool operator >(TokenCursor a, TokenCursor b) => a.Offset > b.Offset;
    
    public static bool operator <(TokenCursor a, TokenCursor b) => a.Offset < b.Offset;

    public static int operator -(TokenCursor a, TokenCursor b) => a.Offset - b.Offset;
}

internal partial record TokenTree(TokenCursor Start, TokenCursor Next)
{
    public readonly List<TokenTree> Children = new();

    public string Production = string.Empty;

    public TokenTree(TokenCursor start) : this(start, start.Next()) { }

    public TokenTree(TokenCursor start, TokenCursor next, IEnumerable<TokenTree> children) 
    : this(start, next) 
    { 
        Children = children.ToList(); 
    }

    public IEnumerable<Token> Tokens()
    {
        var t = Start;

        while (t.More && t.Offset < Next.Offset)
        {
            yield return t.Current;
            t = t.Next();
        }
    }

    public void Dump(TextWriter writer)
    {
        writer.WriteLine($"Line {Start.Line} Col {Start.Col}");
        Dump(writer, string.Empty);
    }

    private void Dump(TextWriter writer, string indent, bool last = true)
    {
        var branch = last ? "└" : "├";
        var prod = Production;
        if (prod != "") prod += ": ";
        var token = Children.Any() ? "" : string.Join("", Tokens().Select(t => t.ToString()));
        writer.WriteLine($"{indent}{branch}─{prod}{token}");
        indent += last ? "  " : "│ ";
        
        foreach (var (child, i) in Children.Select((c, i) => (c, i)))
        {
            child.Dump(writer, indent, i == Children.Count - 1);
        }
    }
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
            var production = _local.Value;
            //if (production == "match") System.Diagnostics.Debugger.Break();
            var start = cursor;
            List<TokenTree> list = new();

            foreach (var parser in parsers)
            {
                var (result, next) = parser(cursor);
                if (result is null) return (null, cursor);

                if (result.Next.Offset > result.Start.Offset) 
                    if (result.Children.Any() && string.IsNullOrEmpty(result.Production))
                        list.AddRange(result.Children);
                    else
                        list.Add(result);

                cursor = next;
            }

            if (list.Count == 1)
                return (list[0], cursor);

            return (new TokenTree(start, cursor, list), cursor);
        };

    public static TokenParser OneOrMore(TokenParser parser) =>
        cursor =>
        {
            var start = cursor;
            List<TokenTree> list = new();
            
            while (true)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;

                if (result.Next.Offset > result.Start.Offset) 
                    if (result.Children.Any() && string.IsNullOrEmpty(result.Production))
                        list.AddRange(result.Children);
                    else
                        list.Add(result);

                cursor = next;
            }
            
            if (cursor.Offset > start.Offset)
            {
                if (list.Count == 1)
                    return (list[0], cursor);
                    
                return (new TokenTree(start, cursor, list), cursor);
            }
                
            return (null, start);
        };

    public static TokenParser ZeroOrMore(TokenParser parser) =>
        cursor =>
        {
            var start = cursor;
            List<TokenTree> list = new();
            
            while (true)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;

                if (result.Next.Offset > result.Start.Offset) 
                    if (result.Children.Any() && string.IsNullOrEmpty(result.Production))
                        list.AddRange(result.Children);
                    else
                        list.Add(result);

                cursor = next;
            }
            
            if (cursor.Offset > start.Offset)
            {
                if (list.Count == 1)
                    return (list[0], cursor);
                    
                return (new TokenTree(start, cursor, list), cursor);
            }
                
            return (new TokenTree(start, start), start);
        };

    public static TokenParser Opt(TokenParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (new TokenTree(cursor, cursor), cursor);
            return (result, next);
        };

    public static TokenParser Delay(Func<TokenParser> delayed) =>
        cursor => delayed()(cursor);

    static System.CodeDom.Compiler.IndentedTextWriter writer = new (Console.Out);

    static AsyncLocal<string> _local = new();

    public static TokenParser Prod(string production, TokenParser parser) =>
        cursor => 
        {
            _local.Value = production;
            //writer.WriteLine($"{production} at {cursor.Offset}");
            //writer.Indent++;
            var (result, next) = parser(cursor);
            //var message = result is null ? $"failed" : $"succeeded";
            //writer.WriteLine($"{production} {message}");
            //writer.Indent--;
            if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = production;
            return (result, next);
        };

    public static TokenParser Ignore(TokenParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (null, cursor);
            result = new TokenTree(next, next, result.Children) { Production = result.Production };
            return (result, next);
        };

    public static TokenParser Merge(TokenParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (null, cursor);
            if (!result.Children.Any()) return (result, next);
            result = new TokenTree(result.Children[0].Start, result.Children[^1].Next) { Production = result.Production };
            return (result, next);
        };
}
