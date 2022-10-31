namespace Plutoscarab.Arafel;

using System.CodeDom.Compiler;
using System.Text;

internal sealed class Program
{
    public static void Main()
    {
        Console.OutputEncoding = Encoding.Unicode;
        var core = new Grammar("core.grammar.txt");
        UpdateLanguage("Core", "core.cs", "core.grammar.txt", core);
        var grammar = new Grammar("grammar.txt");
        UpdateLanguage("Arafel", "language.cs", "grammar.txt", grammar);
        var operators = new Operators("grammar.op.txt");
        Arafel.infixHook = Infix;
        Arafel.prefixHook = Prefix;
        Arafel.postfixHook = Postfix;
        TokenParser parser = Arafel.program;
        //var parser = grammar.GetTokenParsers().Productions["program"];

        Do("sample.af", operators, parser);
        Do("basic.af", operators, parser);
    }

    static void Do(string filename, Operators operators, TokenParser parser)
    {
        var text = File.ReadAllText(filename);
        var tokens = Lexer.Tokenize(text, operators).ToList();
        if (!tokens.Any()) throw new NotImplementedException();
        var cursor = new TokenCursor(tokens, new Context(operators));
        var (parseTree, next) = parser(cursor);
        if (parseTree is null) throw new NotImplementedException();
        Console.WriteLine();
        parseTree.Dump(Console.Out);
        if (next.More) throw new NotImplementedException();
    }

    static (TokenTree? Result, TokenCursor Cursor) Infix(TokenTree? result, TokenCursor cursor)
    {
        if (result is null) return (null, cursor);
        var rightAssoc = result.Children[0].Start.Current.Text == "-";
        var index = rightAssoc ? 1 : 0;

        if (!int.TryParse(result.Children[index].Start.Current.Text, out var precedence))
            return (null, cursor);

        if (precedence < 1 || precedence > 12)
            return (null, cursor);

        var op = result.Children[index + 2].Start.Current.Text;
        return (result, cursor.WithOperator("OP" + precedence, op));
    }

    static void UpdateLanguage(string languageName, string languageFile, string grammarFile, Grammar grammar)
    {
        if (!File.Exists(languageFile) || File.GetLastWriteTime(grammarFile) > File.GetLastWriteTime(languageFile))
        {
            using (var cs = File.CreateText(languageFile))
            using (var writer = new IndentedTextWriter(cs))
            {
                writer.WriteLine("namespace Plutoscarab.Arafel;");
                writer.WriteLine();
                writer.WriteLine($"public sealed partial class {languageName}");
                writer.WriteLine("{");
                writer.Indent++;

                foreach (var production in grammar.Trees.Children)
                {
                    var pname = production.Children[0].Text;
                    writer.WriteLine($"public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> {pname}Hook = (result, cursor) => (result, cursor);");
                    writer.WriteLine();
                    writer.WriteLine($"public static (TokenTree? Result, TokenCursor Next) {pname}(TokenCursor cursor)");
                    writer.WriteLine("{");
                    writer.Indent++;
                    writer.WriteLine($"if (!{pname}_0(ref cursor, out var result)) return (null, cursor);");
                    writer.WriteLine($"if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = \"{pname}\";");
                    writer.WriteLine($"return {pname}Hook(result, cursor);");
                    writer.Indent--;
                    writer.WriteLine("}");
                    writer.WriteLine();
                    var depth = -1;
                    Gen(production.Children[1], pname, ref depth, writer);
                }

                writer.WriteLine("static void Append(List<TokenTree> list, TokenTree? result)");
                writer.WriteLine("{");
                writer.Indent++;
                writer.WriteLine("if (result is not null && result.Next > result.Start)");
                writer.WriteLine("    if (result.Children.Any() && string.IsNullOrEmpty(result.Production))");
                writer.WriteLine("        list.AddRange(result.Children);");
                writer.WriteLine("    else");
                writer.WriteLine("        list.Add(result);");
                writer.Indent--;
                writer.WriteLine("}");
                writer.WriteLine();
                writer.WriteLine("static bool Literal(string literal, ref TokenCursor cursor, out TokenTree? result)");
                writer.WriteLine("{");
                writer.WriteLine("    if (cursor.Current.Text == literal)");
                writer.WriteLine("    {");
                writer.WriteLine("        result = new TokenTree(cursor);");
                writer.WriteLine("        cursor = cursor.Next();");
                writer.WriteLine("        return true;");
                writer.WriteLine("    }");
                writer.WriteLine();
                writer.WriteLine("    result = null;");
                writer.WriteLine("    return false;");
                writer.WriteLine("}");
                writer.WriteLine();
                writer.WriteLine("static bool Production(TokenParser parser, ref TokenCursor cursor, out TokenTree? result)");
                writer.WriteLine("{");
                writer.WriteLine("    (result, cursor) = parser(cursor);");
                writer.WriteLine("    return result is not null;");
                writer.WriteLine("}");
                writer.WriteLine();
                writer.WriteLine("static bool TokenType<T>(ref TokenCursor cursor, out TokenTree? result)");
                writer.WriteLine("{");
                writer.WriteLine("    if (cursor.Current is T)");
                writer.WriteLine("    {");
                writer.WriteLine("        result = new TokenTree(cursor);");
                writer.WriteLine("        cursor = cursor.Next();");
                writer.WriteLine("        return true;");
                writer.WriteLine("    }");
                writer.WriteLine();
                writer.WriteLine("    result = null;");
                writer.WriteLine("    return false;");
                writer.WriteLine("}");
                writer.WriteLine();
                writer.WriteLine("static bool Operator(string op, ref TokenCursor cursor, out TokenTree? result)");
                writer.WriteLine("{");
                writer.WriteLine("    if (cursor.Context.Operators.Contains(op, cursor.Current.Text))");
                writer.WriteLine("    {");
                writer.WriteLine("        result = new TokenTree(cursor);");
                writer.WriteLine("        cursor = cursor.Next();");
                writer.WriteLine("        return true;");
                writer.WriteLine("    }");
                writer.WriteLine();
                writer.WriteLine("    result = null;");
                writer.WriteLine("    return false;");
                writer.WriteLine("}");
                writer.Indent--;
                writer.WriteLine("}");
            }

            throw new Exception();
        }
    }

    static (TokenTree? Result, TokenCursor Cursor) Prefix(TokenTree? result, TokenCursor cursor)
    {
        if (result is null) return (null, cursor);
        var op = result.Children[0].Start.Current.Text;
        return (result, cursor.WithOperator("PREFIX", op));
    }

    static (TokenTree? Result, TokenCursor Cursor) Postfix(TokenTree? result, TokenCursor cursor)
    {
        if (result is null) return (null, cursor);
        var op = result.Children[1].Start.Current.Text;
        return (result, cursor.WithOperator("POSTFIX", op));
    }

    static void Prelude(string pname, ref int depth, IndentedTextWriter writer)
    {
        ++depth;
        writer.WriteLine($"static bool {pname}_{depth}(ref TokenCursor cursor, out TokenTree? result)");
        writer.WriteLine("{");
        writer.Indent++;
    }

    static void Postlude(IndentedTextWriter writer)
    {
        writer.Indent--;
        writer.WriteLine("}");
        writer.WriteLine();
    }

    static string Gen(Token t, string pname, ref int depth, IndentedTextWriter writer)
    {
        var fn = pname + "_" + (depth + 1) + "(";

        switch (t.Production)
        {
            case "seq": Seq(t, pname, ref depth, writer); break;
            case "atom": Atom(t, pname, ref depth, writer); break;
            case "expr": OneOf(t, pname, ref depth, writer); break;
            case "id": return Id(t, pname, ref depth, writer); 
            case "quoted": return $"Literal(\"{t.Text}\", ";
            default: throw new NotImplementedException();
        }

        return fn;
    }

    static Dictionary<string, int> idCache = new();

    static string Id(Token t, string pname, ref int depth, IndentedTextWriter writer)
    {
        if (t.Text.ToUpper() != t.Text)
            return $"Production({t.Text}, ";

        if (Tokens.Lookup.TryGetValue(t.Text, out var _))
        {
            var id = t.Text[0] + t.Text.Substring(1).ToLower();
            return $"TokenType<{id}Token>(";
        }

        return $"Operator(\"{t.Text}\", ";
    }

    static void Atom(Token t, string pname, ref int depth, IndentedTextWriter writer)
    {
        if (t.Children.Count == 2)
        {
            switch (t.Children[1].Text)
            {
                case "+":
                    OneOrMore(t.Children[0], pname, ref depth, writer);
                    break;

                case "*":
                    ZeroOrMore(t.Children[0], pname, ref depth, writer);
                    break;

                case "?":
                    Opt(t.Children[0], pname, ref depth, writer);
                    break;
            }
        }
        else
        {
            Gen(t.Children[0], pname, ref depth, writer);
        }
    }

    static void OneOf(Token t, string pname, ref int depth, IndentedTextWriter writer)
    {
        var d = ++depth;
        var list = new List<string>();

        foreach (var child in t.Children)
        {
            list.Add(Gen(child, pname, ref depth, writer));
        }

        var d2 = depth;
        depth = d - 1;
        Prelude(pname, ref depth, writer);

        foreach (var g in list)
        {
            writer.WriteLine($"if ({g}ref cursor, out result)) return true;");
        }

        writer.WriteLine("result = null;");
        writer.WriteLine("return false;");
        Postlude(writer);
        depth = d2;
    }

    static void Opt(Token t, string pname, ref int depth, IndentedTextWriter writer)
    {
        var d = ++depth;
        var g = Gen(t, pname, ref depth, writer);
        var d2 = depth;
        depth = d - 1;
        Prelude(pname, ref depth, writer);
        writer.WriteLine("var start = cursor;");
        writer.WriteLine();
        writer.WriteLine($"if (!{g}ref cursor, out result))");
        writer.WriteLine("{");
        writer.WriteLine("    cursor = start;");
        writer.WriteLine("    result = new TokenTree(start, start);");
        writer.WriteLine("}");
        writer.WriteLine();
        writer.WriteLine("return true;");
        Postlude(writer);
        depth = d2;
    }

    static void OneOrMore(Token t, string pname, ref int depth, IndentedTextWriter writer)
    {
        var d = ++depth;
        var g = Gen(t, pname, ref depth, writer);
        var d2 = depth;
        depth = d - 1;
        Prelude(pname, ref depth, writer);
        writer.WriteLine("var start = cursor;");
        writer.WriteLine("var list = new List<TokenTree>();");
        writer.WriteLine();
        writer.WriteLine($"while ({g}ref cursor, out result))");
        writer.WriteLine("{");
        writer.Indent++;
        writer.WriteLine("Append(list, result);");
        writer.Indent--;
        writer.WriteLine("}");
        writer.WriteLine();
        writer.WriteLine("if (list.Any())");
        writer.WriteLine("{");
        writer.Indent++;
        writer.WriteLine("if (list.Count == 1) result = list[0];");
        writer.WriteLine("else result = new TokenTree(start, cursor, list);");
        writer.WriteLine("return true;");
        writer.Indent--;
        writer.WriteLine("}");
        writer.WriteLine();
        writer.WriteLine("result = null;");
        writer.WriteLine("return false;");
        Postlude(writer);
        depth = d2;
    }

    static void ZeroOrMore(Token t, string pname, ref int depth, IndentedTextWriter writer)
    {
        var d = ++depth;
        var g = Gen(t, pname, ref depth, writer);
        var d2 = depth;
        depth = d - 1;
        Prelude(pname, ref depth, writer);
        writer.WriteLine("var start = cursor;");
        writer.WriteLine("var list = new List<TokenTree>();");
        writer.WriteLine();
        writer.WriteLine($"while ({g}ref cursor, out result))");
        writer.WriteLine("{");
        writer.Indent++;
        writer.WriteLine("Append(list, result);");
        writer.Indent--;
        writer.WriteLine("}");
        writer.WriteLine();
        writer.WriteLine("if (list.Count == 1) result = list[0];");
        writer.WriteLine("else result = new TokenTree(start, cursor, list);");
        writer.WriteLine("return true;");
        Postlude(writer);
        depth = d2;
    }

    static void Seq(Token t, string pname, ref int depth, IndentedTextWriter writer)
    {
        var d = ++depth;
        var list = new List<string>();

        foreach (var child in t.Children)
        {
            list.Add(Gen(child, pname, ref depth, writer));
        }

        var d2 = depth;
        depth = d - 1;
        Prelude(pname, ref depth, writer);
        writer.WriteLine("var start = cursor;");
        writer.WriteLine("var list = new List<TokenTree>();");

        foreach (var g in list)
        {
            writer.WriteLine($"if (!{g}ref cursor, out result)) {{ cursor = start; return false; }}");
            writer.WriteLine("Append(list, result);");
        }

        writer.WriteLine("if (list.Count == 1) result = list[0];");
        writer.WriteLine("else result = new TokenTree(start, cursor, list);");
        writer.WriteLine("return true;");
        Postlude(writer);
        depth = d2;
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