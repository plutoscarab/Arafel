internal record Cursor(string SourceText, int Offset, int Line, int Col)
{
    public Cursor(string text)
    : this(text, 0, 1, 1)
    {}

    public bool More => Offset < SourceText.Length;

    public char Current => More ? SourceText[Offset] : '\0';

    public Cursor Next()
    {
        if (!More) 
            return this;

        if (Current == '\n')
            return new Cursor(SourceText, Offset + 1, Line + 1, 1);

        if (Current != '\r')
        {
            var surrogate = char.IsSurrogatePair(SourceText, Offset);
            return new Cursor(SourceText, Offset + (surrogate ? 2 : 1), Line, Col + 1);
        }

        var crlf = Offset + 1 < SourceText.Length && SourceText[Offset + 1] == '\n';
        return new Cursor(SourceText, Offset + (crlf ? 2 : 1), Line + 1, 1);
    }

    public static bool operator >(Cursor a, Cursor b) => a.Offset > b.Offset;
    public static bool operator <(Cursor a, Cursor b) => a.Offset < b.Offset;
    public static int operator -(Cursor a, Cursor b) => a.Offset - b.Offset;
}