using System;
using System.CodeDom.Compiler;
using System.IO;
using System.Linq;
using System.Text;

internal sealed class Program
{
    public static void Main()
    {
        var grammar = new Grammar("grammar.txt");
        
        using (var cs = File.CreateText("language.cs"))
        using (var writer = new IndentedTextWriter(cs))
        {
            writer.WriteLine("using static TokenParsers;");
            writer.WriteLine();
            writer.WriteLine("public sealed partial class Arafel");
            writer.WriteLine("{");
            writer.Indent++;

            foreach (var production in grammar.Trees.Children)
            {
                var pname = production.Children[0].Text;
                writer.WriteLine($"public static bool {pname}(ref TokenCursor cursor, out TokenTree? result)");
                writer.WriteLine("{");
                writer.Indent++;
                writer.WriteLine("var list = new List<TokenTree>()");
                writer.WriteLine("var start0 = cursor;");
                Gen(production.Children[1], 1, writer);
                writer.WriteLine("result = null;");
                writer.WriteLine("if (!list.Any()) { cursor = start0; return false; }");
                writer.WriteLine("result = new TokenTree(start0, cursor, list);");
                writer.WriteLine("return true;");
                writer.Indent--;
                writer.WriteLine("}");
                writer.WriteLine();
            }

            writer.Indent--;
            writer.WriteLine("}");
        }

        var parsers = grammar.GetTokenParsers();
        parsers.Productions["infix"] = Infix(parsers.Productions["infix"]);
        parsers.Productions["postfix"] = Postfix(parsers.Productions["postfix"]);
        var parser = parsers.Productions["program"];
        parser = Arafel.program;
        var text = File.ReadAllText("sample.af");
        var operators = new Operators("grammar.op.txt");
        var tokens = Lexer.Tokenize(text, operators).ToList();
        if (!tokens.Any()) throw new NotImplementedException();
        var cursor = new TokenCursor(tokens, new Context(operators));
        var (result, next) = parser(cursor);
        if (result is null) throw new NotImplementedException();
        Console.OutputEncoding = Encoding.Unicode;
        Console.WriteLine();
        result.Dump(Console.Out);
        if (next.More) throw new NotImplementedException();
    }

    static TokenParser Infix(TokenParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (null, cursor);
            var rightAssoc = result.Children[0].Start.Current.Text == "-";
            var index = rightAssoc ? 1 : 0;

            if (!int.TryParse(result.Children[index].Start.Current.Text, out var precedence))
                return (null, cursor);

            if (precedence < 1 || precedence > 12)
                return (null, cursor);

            var op = result.Children[index + 2].Start.Current.Text;
            return (result, next.WithOperator("OP" + precedence, op));
        };

    static TokenParser Prefix(TokenParser parser)
    {
        return parser;
    }

    static TokenParser Postfix(TokenParser parser) =>
        cursor =>
        {
            var (result, next) = parser(cursor);
            if (result is null) return (null, cursor);
            var op = result.Children[1].Start.Current.Text;
            return (result, next.WithOperator("POSTFIX", op));
        };

    static void Gen(Token t, int depth, IndentedTextWriter writer)
    {
        switch (t.Production)
        {
            case "seq": Seq(t, depth, writer); return;
            case "atom": Atom(t, depth, writer); return;
            case "expr": OneOf(t, depth, writer); return;
            case "id": Id(t, depth, writer); return;
            case "quoted": Quoted(t, depth, writer); return;
        }

        throw new NotImplementedException();
    }

    static void Quoted(Token t, int depth, IndentedTextWriter writer)
    {
        writer.WriteLine($"if (cursor.Current.Text == \"{t.Text}\")");
        writer.WriteLine("{");
        writer.Indent++;
        writer.WriteLine("list.Add(new TokenTree(cursor));");
        writer.WriteLine("cursor = cursor.Next();");
        writer.Indent--;
        writer.WriteLine("}");
    }

    static void Id(Token t, int depth, IndentedTextWriter writer)
    {
        if (t.Text.ToUpper() != t.Text)
        {
            writer.WriteLine($"if {t.Text}(ref cursor, out result)");
            writer.WriteLine("    list.Add(result);");
            return;
        }

        if (Tokens.Lookup.TryGetValue(t.Text, out var _))
        {
            var id = t.Text[0] + t.Text.Substring(1).ToLower();
            writer.WriteLine($"if (cursor.Current is {id}Token)");
            writer.WriteLine("{");
            writer.Indent++;
            writer.WriteLine("list.Add(new TokenTree(cursor));");
            writer.WriteLine("cursor = cursor.Next();");
            writer.Indent--;
            writer.WriteLine("}");
            return;
        }

        writer.WriteLine($"if (!cursor.Context.Operators.Contains(\"{t.Text}\", cursor.Current.Text))");
        writer.WriteLine("    return false;");
        writer.WriteLine();
        writer.WriteLine("result = new TokenTree(cursor);");
        writer.WriteLine("cursor = cursor.Next();");
        writer.WriteLine("return true;");
    }

    static void Atom(Token t, int depth, IndentedTextWriter writer)
    {
        if (t.Children.Count == 2)
        {
            switch (t.Children[1].Text)
            {
                case "+":
                    OneOrMore(t.Children[0], depth + 1, writer);
                    break;

                case "*":
                    ZeroOrMore(t.Children[0], depth + 1, writer);
                    break;

                case "?":
                    Opt(t.Children[0], depth + 1, writer);
                    break;
            }
        }
        else
        {
            Gen(t.Children[0], depth + 1, writer);
        }
    }

    static void OneOf(Token t, int depth, IndentedTextWriter writer)
    {
        writer.Indent++;

        foreach (var child in t.Children)
        {
            Gen(child, depth + 1, writer);
            writer.WriteLine("if (result is not null) break;");
        }

        writer.Indent--;
    }

    static void Opt(Token t, int depth, IndentedTextWriter writer)
    {
        Gen(t, depth + 1, writer);
        writer.WriteLine("if (result is null) result = new TokenTree(cursor, cursor);");
        writer.WriteLine("else cursor = next;");
    }

    static void OneOrMore(Token t, int depth, IndentedTextWriter writer)
    {
        writer.WriteLine("{");
        writer.Indent++;
        writer.WriteLine($"var start{depth} = cursor;");
        writer.WriteLine($"var list{depth} = new List<TokenTree>();");
        writer.WriteLine("while (true)");
        writer.WriteLine("{");
        writer.Indent++;
        Gen(t, depth + 1, writer);
        writer.WriteLine("if (result is null) break;");
        writer.WriteLine($"list{depth}.Add(result);");
        writer.WriteLine("cursor = next;");
        writer.Indent--;
        writer.WriteLine("}");
        writer.WriteLine($"if (list{depth}.Any())");
        writer.WriteLine($"    result = new TokenTree(start{depth}, cursor, list{depth});");
        writer.WriteLine("else");
        writer.WriteLine($"    cursor = start{depth};");
        writer.Indent--;
        writer.WriteLine("}");
    }

    static void ZeroOrMore(Token t, int depth, IndentedTextWriter writer)
    {
        writer.WriteLine("{");
        writer.Indent++;
        writer.WriteLine($"var start{depth} = cursor;");
        writer.WriteLine($"var list{depth} = new List<TokenTree>();");
        writer.WriteLine("while (true)");
        writer.WriteLine("{");
        writer.Indent++;
        Gen(t, depth + 1, writer);
        writer.WriteLine("if (result is null) break;");
        writer.WriteLine($"list{depth}.Add(result);");
        writer.WriteLine("cursor = next;");
        writer.Indent--;
        writer.WriteLine("}");
        writer.WriteLine($"result = new TokenTree(start{depth}, cursor, list{depth});");
        writer.Indent--;
        writer.WriteLine("}");
    }

    static void Seq(Token t, int depth, IndentedTextWriter writer)
    {
        writer.WriteLine("{");
        writer.Indent++;
        writer.WriteLine($"var start{depth} = cursor;");
        writer.WriteLine($"var list{depth} = new List<TokenTree>();");
        writer.WriteLine("do");
        writer.WriteLine("{");
        writer.Indent++;

        foreach (var child in t.Children)
        {
            Gen(child, depth + 1, writer);
            writer.WriteLine("if (result is null) break;");
            writer.WriteLine($"if (result.Next > result.Start) list{depth}.Add(result);");
            writer.WriteLine("cursor = next;");
        }

        writer.WriteLine($"result = list{depth}.Count == 1 ? list{depth}[0] : new TokenTree(start{depth}, cursor, list{depth});");
        writer.Indent--;
        writer.WriteLine("}");
        writer.WriteLine("while (false);");
        writer.WriteLine($"if (result is null) cursor = start{depth};");
        writer.Indent--;
        writer.WriteLine("}");
    }

    static void Generate(Token t, IndentedTextWriter writer)
    {
        var sep = string.Empty;
        switch (t.Production)
        {
            case "seq":
                writer.Write("Seq(");
                foreach (var child in t.Children)
                {
                    writer.Write(sep);
                    Generate(child, writer);
                    sep = ", ";
                }
                writer.Write(")");
                return;

            case "expr":
                writer.Write("OneOf(");
                foreach (var child in t.Children)
                {
                    writer.Write(sep);
                    Generate(child, writer);
                    sep = ", ";
                }
                writer.Write(")");
                return;

            case "quoted":
                writer.Write($"Expect(\"{t.Text}\")");
                return;
                
            case "id":
                if (t.Text != t.Text.ToUpperInvariant())
                {
                    writer.Write(t);
                    return;
                }

                if (Tokens.Lookup.TryGetValue(t.Text, out var _))
                {
                    writer.Write("Expect<");
                    writer.Write(t.Text[0]);
                    writer.Write(t.Text.Substring(1).ToLowerInvariant());
                    writer.Write("Token>()");
                    return;
                }

                writer.Write("Operator(\"");
                writer.Write(t.Text);
                writer.Write("\")");
                return;

            case "atom":
                if (t.Children.Count == 2)
                {
                    var modifier = t.Children[1].ToString();

                    switch (modifier)
                    {
                        case "+":
                            writer.Write("OneOrMore(");
                            Generate(t.Children[0], writer);
                            writer.Write(")");
                            return;

                        case "*":
                            writer.Write("ZeroOrMore(");
                            Generate(t.Children[0], writer);
                            writer.Write(")");
                            return;

                        case "?":
                            writer.Write("Opt(");
                            Generate(t.Children[0], writer);
                            writer.Write(")");
                            return;
                    }

                    throw new NotImplementedException();
                }

                Generate(t.Children[0], writer);
                return;

            case "charCode":
                //return CompileCharCode(t);
                break;

            case "charOrCode":
                //return CompileCharOrCode(t);
                break;

            case "charSet":
                //return CompileCharSet(t);
                break;
        }

        throw new NotImplementedException();
    }
}