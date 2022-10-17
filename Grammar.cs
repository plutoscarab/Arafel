using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

using static RuneParsers;

public sealed partial class Grammar
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
    static RuneParser charCode = Prod("charCode", Seq(Ignore(Str("#x")), Merge(OneOrMore(OneOf("0123456789ABCDEFabcdef".Select(Char).ToArray())))));
    static RuneParser charOrCode = Prod("charOrCode", OneOf(charCode, Expect(r => r.ToString() != "]")));
    static RuneParser charRange = Prod("charRange", Seq(charOrCode, Ignore(Char('-')), charOrCode));
    static RuneParser charSet = Prod("charSet", Seq(Ignore(Char('[')), Opt(Char('^')), OneOrMore(OneOf(charRange, charOrCode)), Ignore(Char(']'))));
    static RuneParser atom = Prod("atom", Seq(ws, OneOf(id, quoted, parens, charCode, charSet), Opt(Prod("modifier", OneOf(Char('?'), Char('*'), Char('+'))))));
    static RuneParser sequence = Prod("seq", OneOrMore(atom));
    static RuneParser production = Prod("production", Seq(ws, id, ws, Ignore(Str("::=")), ws, expr, lb));
    static RuneParser grammar = Prod("grammar", OneOrMore(production));

    static Grammar()
    {
        exprDelayed = Seq(ZeroOrMore(Seq(ws, sequence, ws, Ignore(Char('|')))), sequence);
    }

    public readonly Token Trees;

    public Grammar(string filename)
    {
        var text = File.ReadAllText(filename);
        var cursor = new Cursor(text, new Context(new Operators()));
        var (result, next) = grammar(cursor);
        if (result is null) throw new NotImplementedException();
        if (next.Offset != text.Length) throw new NotImplementedException();
        Trees = result;
    }
}

internal static class Runes
{
    public static readonly Dictionary<string, RuneParser> Lookup = new()
    {
        { "UDIGIT", RuneParsers.Expect(Rune.IsDigit) },
        { "ULETTER", RuneParsers.Expect(Rune.IsLetter) },
    };
}