using System.Reflection;

internal record Token(Cursor Start, int Length)
{
    static Dictionary<string, Type> TokenTypes = 
        Assembly.GetExecutingAssembly().GetTypes().Where(t => typeof(Token).IsAssignableFrom(t)).ToDictionary(
            t => t.Name.Replace("Token", ""), t => t, StringComparer.InvariantCultureIgnoreCase);

    public Token() : this(Cursor.Empty, 0) { }

    public Token(Cursor start) : this(start, 1) { }

    public Token(Cursor start, Cursor next) : this(start, next - start) { }

    public string Text => 
        Start.SourceText.Substring(Start.Offset, Length);

    public override string ToString() => 
        $"Line {Start.Line} Col {Start.Col} '{Text}'";
}
