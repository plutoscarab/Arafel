internal sealed record IdentifierToken(Cursor Start, int Length)
: Token(Start, Length);