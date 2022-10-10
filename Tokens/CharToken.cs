internal sealed record CharToken(Cursor Start, int Length)
: Token(Start, Length);