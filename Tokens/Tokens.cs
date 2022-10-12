internal sealed record EndToken() : Token(Cursor.Empty, 0)
{
    public static readonly EndToken Instance = new EndToken();
}

internal sealed record LParenToken(Cursor Start) : Token(Start, 1);

internal sealed record RParenToken(Cursor Start) : Token(Start, 1);

internal sealed record LBrackToken(Cursor Start) : Token(Start, 1);

internal sealed record RBrackToken(Cursor Start) : Token(Start, 1);

internal sealed record CommaToken(Cursor Start) : Token(Start, 1);

internal sealed record CharToken(Cursor Start, int Length) : Token(Start, Length);

internal sealed record IdToken(Cursor Start, int Length) : Token(Start, Length);

internal sealed record KeywordToken(Cursor Start, int Length) : Token(Start, Length);

internal sealed record OperatorToken(Cursor Start, int Length) : Token(Start, Length);

internal sealed record StringToken(Cursor Start, int Length) : Token(Start, Length);

internal sealed record BoolToken(Cursor Start, int Length) : Token(Start, Length);

internal sealed record UnknownToken(Cursor Start, int Length) : Token(Start, Length);

internal sealed record SuperToken(Cursor Start, int Length) : Token(Start, Length);

internal sealed record DecimalToken(Cursor Start, int Length) : Token(Start, Length);


