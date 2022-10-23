using System.Collections.Generic;
internal sealed partial record EndToken() : Token(Cursor.Empty, Cursor.Empty)
{
    public static readonly EndToken Instance = new EndToken();
}

internal sealed partial record LParenToken(Cursor Start) 
: Token(Start, Start.Next())
{
    public override string ToString() => Text;
}

internal sealed partial record RParenToken(Cursor Start) 
: Token(Start, Start.Next())
{
    public override string ToString() => Text;
}

internal sealed partial record LBrackToken(Cursor Start) 
: Token(Start, Start.Next())
{
    public override string ToString() => Text;
}

internal sealed partial record RBrackToken(Cursor Start) 
: Token(Start, Start.Next())
{
    public override string ToString() => Text;
}

internal sealed partial record CommaToken(Cursor Start) 
: Token(Start, Start.Next())
{
    public override string ToString() => Text;
}

internal sealed partial record LBraceToken(Cursor Start) 
: Token(Start, Start.Next())
{
    public override string ToString() => Text;
}

internal sealed partial record RBraceToken(Cursor Start) 
: Token(Start, Start.Next())
{
    public override string ToString() => Text;
}

internal partial record LiteralToken(Cursor Start, Cursor Next) 
: Token(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record IdToken(Cursor Start, Cursor Next) 
: Token(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record KeywordToken(Cursor Start, Cursor Next) 
: Token(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record OperatorToken(Cursor Start, Cursor Next) 
: Token(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record UnknownToken(Cursor Start, Cursor Next) 
: Token(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record SuperToken(Cursor Start, Cursor Next) 
: Token(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record NatListToken(Cursor Start, Cursor Next) 
: Token(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record CharToken(Cursor Start, Cursor Next) 
: LiteralToken(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record StringToken(Cursor Start, Cursor Next) 
: LiteralToken(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record BoolToken(Cursor Start, Cursor Next) 
: LiteralToken(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record NatToken(Cursor Start, Cursor Next) 
: LiteralToken(Start, Next)
{
    public override string ToString() => Text;
}

internal sealed partial record DecimalToken(Cursor Start, Cursor Next) 
: LiteralToken(Start, Next)
{
    public override string ToString() => Text;
}


internal static class Tokens
{
    public static readonly Dictionary<string, TokenParser> Lookup = new()
    {
        { "END", TokenParsers.Expect<EndToken>() },
        { "LPAREN", TokenParsers.Expect<LParenToken>() },
        { "RPAREN", TokenParsers.Expect<RParenToken>() },
        { "LBRACK", TokenParsers.Expect<LBrackToken>() },
        { "RBRACK", TokenParsers.Expect<RBrackToken>() },
        { "COMMA", TokenParsers.Expect<CommaToken>() },
        { "LBRACE", TokenParsers.Expect<LBraceToken>() },
        { "RBRACE", TokenParsers.Expect<RBraceToken>() },
        { "LITERAL", TokenParsers.Expect<LiteralToken>() },
        { "ID", TokenParsers.Expect<IdToken>() },
        { "KEYWORD", TokenParsers.Expect<KeywordToken>() },
        { "OPERATOR", TokenParsers.Expect<OperatorToken>() },
        { "UNKNOWN", TokenParsers.Expect<UnknownToken>() },
        { "SUPER", TokenParsers.Expect<SuperToken>() },
        { "NATLIST", TokenParsers.Expect<NatListToken>() },
        { "CHAR", TokenParsers.Expect<CharToken>() },
        { "STRING", TokenParsers.Expect<StringToken>() },
        { "BOOL", TokenParsers.Expect<BoolToken>() },
        { "NAT", TokenParsers.Expect<NatToken>() },
        { "DECIMAL", TokenParsers.Expect<DecimalToken>() },
    };
}
