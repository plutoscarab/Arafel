internal sealed record StringToken(Cursor Start, int Length)
: Token(Start, Length);