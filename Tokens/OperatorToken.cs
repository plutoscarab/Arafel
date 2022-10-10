internal sealed record OperatorToken(Cursor Start, int Length)
: Token(Start, Length);