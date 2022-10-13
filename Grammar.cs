using System.Text;
using static RuneParsers;

internal sealed class Grammar
{
    static RuneParser Char(char ch) => Expect(r => r.ToString() == ch.ToString());
    static RuneParser Str(string s) => Prod(s, Seq(s.Select(c => Char(c)).ToArray()));
    static RuneParser ws = Ignore(Prod("ws", ZeroOrMore(Char(' '))));
    static RuneParser lb = Ignore(Prod("lb", OneOrMore(OneOf(Char('\r'), Char('\n')))));
    static RuneParser id = Merge(Prod("id", Seq(Expect(r => Rune.IsLetter(r) || r.ToString() == "_"), ZeroOrMore(Expect(r => Rune.IsLetter(r) || Rune.IsDigit(r) || r.ToString() == "_")))));
    static RuneParser quoted = Merge(Prod("quoted", Seq(ws, Ignore(Char('\'')), OneOrMore(Expect(r => r.ToString() != "'")), Ignore(Char('\'')))));
    static RuneParser exprDelayed = Any;
    static RuneParser expr = Prod("expr", Delay(() => exprDelayed));
    static RuneParser parens = Seq(ws, Ignore(Char('(')), expr, ws, Ignore(Char(')')));
    static RuneParser atom = Prod("atom", Seq(ws, OneOf(id, quoted, parens), Opt(Prod("modifier", OneOf(Char('?'), Char('*'), Char('+'))))));
    static RuneParser sequence = Prod("seq", OneOrMore(atom));
    static RuneParser production = Prod("production", Seq(ws, id, ws, Ignore(Str("::=")), ws, expr, lb));
    static RuneParser grammar = Prod("grammar", OneOrMore(production));

    static Grammar()
    {
        exprDelayed = Seq(ZeroOrMore(Seq(ws, sequence, ws, Ignore(Char('|')))), sequence);
    }

    Dictionary<string, TokenParser> productions;

    public readonly TokenParser Program;

    public Grammar(string filename)
    {
        var text = File.ReadAllText(filename);
        var cursor = new Cursor(text);
        var (result, next) = grammar(cursor);
        if (result is null) throw new NotImplementedException();
        if (next.Offset != text.Length) throw new NotImplementedException();

        productions = result.Children.ToDictionary(
            p => p.Children[0].Text, 
            p => TokenParsers.Prod(p.Children[0].Text, Compile(p.Children[1])));

        Program = productions["program"];
        Console.OutputEncoding = Encoding.Unicode;

        foreach (var tree in result.Children)
        {
            Console.WriteLine();
            tree.Dump(Console.Out);
        }
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
                return TokenParsers.Expect(to => to.Text == t.Text);

            case "id":
                if (t.Text != t.Text.ToUpperInvariant())
                    return TokenParsers.Delay(() => productions[t.Text]);

                return Tokens.Lookup[t.Text];

            case "atom":
                var expr = Compile(t.Children[0]);

                if (t.Children.Count == 2)
                {
                    var modifier = t.Children[1].Text;

                    switch (modifier)
                    {
                        case "?": return TokenParsers.Opt(expr);
                        case "*": return TokenParsers.ZeroOrMore(expr);
                        case "+": return TokenParsers.OneOrMore(expr);
                        default: throw new NotImplementedException();
                    }
                }

                return expr;
        }

        throw new NotImplementedException();
    }
}