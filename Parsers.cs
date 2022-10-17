using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Text;

public sealed partial record Cursor(IReadOnlyList<Rune> Source, int Offset, Context Context)
{
    public Cursor(IList<Rune> source, Context context)
    : this((IReadOnlyList<Rune>)source, 0, context)
    { }

    private Cursor(Cursor cursor, int offset)
    : this(cursor.Source, offset, cursor.Context)
    { }

    public bool More => Offset < Source.Count;

    public Rune Current => More ? Source[Offset] : new Rune();

    public static bool operator >(Cursor a, Cursor b) => a.Offset > b.Offset;
    
    public static bool operator <(Cursor a, Cursor b) => a.Offset < b.Offset;

    public static int operator -(Cursor a, Cursor b) => a.Offset - b.Offset;

    public Cursor WithOperator(string production, string op)
    {
        return new Cursor(Source, Offset, Context.WithOperator(production, op));
    }
}

public partial record Token(Cursor Start, Cursor Next)
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

public delegate (Token? Result, Cursor Next) RuneParser(Cursor cursor);

internal sealed class RuneParsers
{
    public static RuneParser Any =
        cursor => (new Token(cursor), cursor.Next());

    public static RuneParser Expect(Predicate<Rune> predicate) =>
        cursor => predicate(cursor.Current) ? (new Token(cursor), cursor.Next()) : (null, cursor);

    public static RuneParser Expect<T>() =>
        Expect(rune => rune is T);

    public static RuneParser Expect<T>(params string[] choices) =>
        Expect(rune => rune is T && choices.Contains(rune.ToString()));

    public static RuneParser Expect(params Rune[] values) =>
        Expect(rune => values.Contains(rune));

    public static RuneParser Expect(string text) =>
        Expect(t => t.ToString() == text);

    public static RuneParser Operator(string tag) =>
        cursor => cursor.Context.Operators.GetRuneParser(tag)(cursor);

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

    private static void Append(List<Token> list, Token result)
    {
        if (result.Next.Offset <= result.Start.Offset)
            return;

        if (result.Production.EndsWith("_"))
            return;

        if (!result.Children.Any() || !string.IsNullOrEmpty(result.Production))
        {
            list.Add(result);
            return;
        }

        list.AddRange(result.Children.Where(r => !r.Production.EndsWith("_")));
    }

    public static RuneParser Seq(params RuneParser[] parsers) =>
        cursor =>
        {
            var start = cursor;
            List<Token> list = new();

            foreach (var parser in parsers)
            {
                var (result, next) = parser(cursor);
                if (result is null) return (null, cursor);
                Append(list, result);
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
            
            while (cursor.More)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;
                Append(list, result);
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
            
            while (cursor.More)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;
                Append(list, result);
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

    public static RuneParser Prod(string production, RuneParser parser) =>
        cursor => 
        {
            var (result, next) = parser(cursor);
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

    public static RuneParser Not(RuneParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (new Token(cursor, cursor.Next()), cursor.Next());
            return (null, cursor);
        };
}

public sealed partial class Grammar
{
    public RuneGrammar GetRuneParsers()
    {
        Console.OutputEncoding = Encoding.Unicode;
        Console.WriteLine();
        Trees.Dump(Console.Out);
        return new RuneGrammar(Trees);
    }
}

public sealed partial class RuneGrammar
{
    public readonly Dictionary<string, RuneParser> Productions;

    public RuneGrammar(Token t)
    {
        Productions = t.Children.ToDictionary(
            p => p.Children[0].ToString(), 
            p => RuneParsers.Prod(p.Children[0].ToString(), Compile(p.Children[1])))
        .ToDictionary(
            p => p.Key,
            p => char.IsUpper(p.Key[0]) ? RuneParsers.Merge(p.Value) : p.Value);
    }

    RuneParser Compile(Token t)
    {
        switch (t.Production)
        {
            case "seq":
                return RuneParsers.Seq(t.Children.Select(Compile).ToArray());

            case "expr":
                return RuneParsers.OneOf(t.Children.Select(Compile).ToArray());

            case "quoted":
                return RuneParsers.Expect(to => to.ToString() == t.Text);

            case "id":
                if (t.Text != t.Text.ToUpperInvariant())
                    return RuneParsers.Delay(() => Productions[t.ToString()]);
 
                if (Runes.Lookup.TryGetValue(t.Text, out var parser))
                    return parser;

                return cursor => cursor.Context.Operators.GetRuneParser(t.Text)(cursor);

            case "atom":
                var expr = Compile(t.Children[0]);

                if (t.Children.Count == 2)
                {
                    var modifier = t.Children[1].ToString();

                    switch (modifier)
                    {
                        case "?": return RuneParsers.Opt(expr);
                        case "*": return RuneParsers.ZeroOrMore(expr);
                        case "+": return RuneParsers.OneOrMore(expr);
                        default: throw new NotImplementedException();
                    }
                }

                return expr;

            case "charCode":
                return CompileCharCode(t);

            case "charOrCode":
                return CompileCharOrCode(t);

            case "charSet":
                return CompileCharSet(t);
        }

        throw new NotImplementedException();
    }

    static RuneParser CompileCharCode(Token t)
    {
        var cp = int.Parse(t.ToString(), System.Globalization.NumberStyles.HexNumber);
        return RuneParsers.Expect(r => r.ToString() == char.ConvertFromUtf32(cp));
    }

    static int GetCharOrCode(Token t)
    {
        if (t.Production == "charCode")
            return int.Parse(t.ToString(), System.Globalization.NumberStyles.HexNumber);

        return char.ConvertToUtf32(t.ToString(), 0);
    }

    static RuneParser CompileCharOrCode(Token t)
    {
        return RuneParsers.Expect(r => r.ToString() == char.ConvertFromUtf32(GetCharOrCode(t)));
    }

    static RuneParser CompileRange(Token t)
    {
        var start = GetCharOrCode(t.Children[0]);
        var end = GetCharOrCode(t.Children[1]);

        return RuneParsers.Expect(r => {
            var cp = char.ConvertToUtf32(r.ToString(), 0);
            return cp >= start && cp <= end;
        });
    }

    static RuneParser CompileCodeOrRange(Token t)
    {
        if (t.Production == "charCode")
            return CompileCharCode(t);

        if (t.Children.Any())
            return CompileRange(t);

        return RuneParsers.Expect(r => r.ToString() == t.ToString());
    }

    static RuneParser CompileCharSet(Token t)
    {
        var neg = t.Children[0].ToString() == "^";
        var chars = t.Children.Skip(neg ? 1 : 0).Select(CompileCodeOrRange).ToArray();
        var p = chars.Length == 1 ? chars[0] : RuneParsers.OneOf(chars);
        if (neg) p = RuneParsers.Not(p);
        return p;
    }
}

public sealed partial class Operators
{
    public RuneParser GetRuneParser(string id) =>
        cursor =>
        {
            if (lookup[id].Contains(cursor.Current.ToString()))
                return (new Token(cursor), cursor.Next());

            return (null, cursor);
        };
}

public sealed partial record TokenCursor(IReadOnlyList<Token> Source, int Offset, Context Context)
{
    public TokenCursor(IList<Token> source, Context context)
    : this((IReadOnlyList<Token>)source, 0, context)
    { }

    private TokenCursor(TokenCursor cursor, int offset)
    : this(cursor.Source, offset, cursor.Context)
    { }

    public bool More => Offset < Source.Count;

    public Token Current => More ? Source[Offset] : new Token(Cursor.Empty, Cursor.Empty);

    public static bool operator >(TokenCursor a, TokenCursor b) => a.Offset > b.Offset;
    
    public static bool operator <(TokenCursor a, TokenCursor b) => a.Offset < b.Offset;

    public static int operator -(TokenCursor a, TokenCursor b) => a.Offset - b.Offset;

    public TokenCursor WithOperator(string production, string op)
    {
        return new TokenCursor(Source, Offset, Context.WithOperator(production, op));
    }
}

public partial record TokenTree(TokenCursor Start, TokenCursor Next)
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

public delegate (TokenTree? Result, TokenCursor Next) TokenParser(TokenCursor cursor);

internal sealed class TokenParsers
{
    public static TokenParser Any =
        cursor => (new TokenTree(cursor), cursor.Next());

    public static TokenParser Expect(Predicate<Token> predicate) =>
        cursor => predicate(cursor.Current) ? (new TokenTree(cursor), cursor.Next()) : (null, cursor);

    public static TokenParser Expect<T>() =>
        Expect(token => token is T);

    public static TokenParser Expect<T>(params string[] choices) =>
        Expect(token => token is T && choices.Contains(token.ToString()));

    public static TokenParser Expect(params Token[] values) =>
        Expect(token => values.Contains(token));

    public static TokenParser Expect(string text) =>
        Expect(t => t.ToString() == text);

    public static TokenParser Operator(string tag) =>
        cursor => cursor.Context.Operators.GetTokenParser(tag)(cursor);

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

    private static void Append(List<TokenTree> list, TokenTree result)
    {
        if (result.Next.Offset <= result.Start.Offset)
            return;

        if (result.Production.EndsWith("_"))
            return;

        if (!result.Children.Any() || !string.IsNullOrEmpty(result.Production))
        {
            list.Add(result);
            return;
        }

        list.AddRange(result.Children.Where(r => !r.Production.EndsWith("_")));
    }

    public static TokenParser Seq(params TokenParser[] parsers) =>
        cursor =>
        {
            var start = cursor;
            List<TokenTree> list = new();

            foreach (var parser in parsers)
            {
                var (result, next) = parser(cursor);
                if (result is null) return (null, cursor);
                Append(list, result);
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
            
            while (cursor.More)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;
                Append(list, result);
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
            
            while (cursor.More)
            {
                var (result, next) = parser(cursor);
                if (result is null) break;
                Append(list, result);
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

    public static TokenParser Prod(string production, TokenParser parser) =>
        cursor => 
        {
            var (result, next) = parser(cursor);
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

    public static TokenParser Not(TokenParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (new TokenTree(cursor, cursor.Next()), cursor.Next());
            return (null, cursor);
        };
}

public sealed partial class Grammar
{
    public TokenGrammar GetTokenParsers()
    {
        Console.OutputEncoding = Encoding.Unicode;
        Console.WriteLine();
        Trees.Dump(Console.Out);
        return new TokenGrammar(Trees);
    }
}

public sealed partial class TokenGrammar
{
    public readonly Dictionary<string, TokenParser> Productions;

    public TokenGrammar(Token t)
    {
        Productions = t.Children.ToDictionary(
            p => p.Children[0].ToString(), 
            p => TokenParsers.Prod(p.Children[0].ToString(), Compile(p.Children[1])))
        .ToDictionary(
            p => p.Key,
            p => char.IsUpper(p.Key[0]) ? TokenParsers.Merge(p.Value) : p.Value);
    }

    TokenParser Compile(Token t)
    {
        switch (t.Production)
        {
            case "seq":
                return TokenParsers.Seq(t.Children.Select(Compile).ToArray());

            case "expr":
                return TokenParsers.OneOf(t.Children.Select(Compile).ToArray());

            case "quoted":
                return TokenParsers.Expect(to => to.ToString() == t.Text);

            case "id":
                if (t.Text != t.Text.ToUpperInvariant())
                    return TokenParsers.Delay(() => Productions[t.ToString()]);
 
                if (Tokens.Lookup.TryGetValue(t.Text, out var parser))
                    return parser;

                return cursor => cursor.Context.Operators.GetTokenParser(t.Text)(cursor);

            case "atom":
                var expr = Compile(t.Children[0]);

                if (t.Children.Count == 2)
                {
                    var modifier = t.Children[1].ToString();

                    switch (modifier)
                    {
                        case "?": return TokenParsers.Opt(expr);
                        case "*": return TokenParsers.ZeroOrMore(expr);
                        case "+": return TokenParsers.OneOrMore(expr);
                        default: throw new NotImplementedException();
                    }
                }

                return expr;

            case "charCode":
                return CompileCharCode(t);

            case "charOrCode":
                return CompileCharOrCode(t);

            case "charSet":
                return CompileCharSet(t);
        }

        throw new NotImplementedException();
    }

    static TokenParser CompileCharCode(Token t)
    {
        var cp = int.Parse(t.ToString(), System.Globalization.NumberStyles.HexNumber);
        return TokenParsers.Expect(r => r.ToString() == char.ConvertFromUtf32(cp));
    }

    static int GetCharOrCode(Token t)
    {
        if (t.Production == "charCode")
            return int.Parse(t.ToString(), System.Globalization.NumberStyles.HexNumber);

        return char.ConvertToUtf32(t.ToString(), 0);
    }

    static TokenParser CompileCharOrCode(Token t)
    {
        return TokenParsers.Expect(r => r.ToString() == char.ConvertFromUtf32(GetCharOrCode(t)));
    }

    static TokenParser CompileRange(Token t)
    {
        var start = GetCharOrCode(t.Children[0]);
        var end = GetCharOrCode(t.Children[1]);

        return TokenParsers.Expect(r => {
            var cp = char.ConvertToUtf32(r.ToString(), 0);
            return cp >= start && cp <= end;
        });
    }

    static TokenParser CompileCodeOrRange(Token t)
    {
        if (t.Production == "charCode")
            return CompileCharCode(t);

        if (t.Children.Any())
            return CompileRange(t);

        return TokenParsers.Expect(r => r.ToString() == t.ToString());
    }

    static TokenParser CompileCharSet(Token t)
    {
        var neg = t.Children[0].ToString() == "^";
        var chars = t.Children.Skip(neg ? 1 : 0).Select(CompileCodeOrRange).ToArray();
        var p = chars.Length == 1 ? chars[0] : TokenParsers.OneOf(chars);
        if (neg) p = TokenParsers.Not(p);
        return p;
    }
}

public sealed partial class Operators
{
    public TokenParser GetTokenParser(string id) =>
        cursor =>
        {
            if (lookup[id].Contains(cursor.Current.ToString()))
                return (new TokenTree(cursor), cursor.Next());

            return (null, cursor);
        };
}
