internal sealed record EndToken() 
: Token(new Cursor(string.Empty), 0)
{
    public static readonly EndToken Instance = new EndToken();
}
