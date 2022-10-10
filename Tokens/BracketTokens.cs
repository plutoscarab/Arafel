
internal sealed record OpenParensToken(Cursor Start)
: Token(Start, 1);

internal sealed record CloseParensToken(Cursor Start)
: Token(Start, 1);

internal sealed record OpenBracketToken(Cursor Start)
: Token(Start, 1);

internal sealed record CloseBracketToken(Cursor Start)
: Token(Start, 1);