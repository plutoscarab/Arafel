
internal record Tursor(IList<Token> Tokens, int Offset)
{
    public Tursor(IList<Token> tokens)
    : this(tokens, 0)
    { }

    public bool More => Offset < Tokens.Count;

    public Token Current => More ? Tokens[Offset] : EndToken.Instance;

    public Tursor Next()
    {
        if (!More)
            return this;

        return new Tursor(Tokens, Offset + 1);
    }

    public static int operator -(Tursor a, Tursor b) =>
        a.Offset - b.Offset;
}