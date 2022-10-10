
internal sealed record UnknownToken(Cursor Start, int Length)
: Token(Start, Length)
{
    public override string ToString() =>
        base.ToString();
}