internal sealed record SeparatorToken(Cursor Start)
: Token(Start, 1);