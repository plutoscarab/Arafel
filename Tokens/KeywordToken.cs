internal sealed record KeywordToken(Cursor Start, int Length)
: Token(Start, Length);