using System.Text;

internal partial record Cursor
{
    public static Cursor Empty = new Cursor(string.Empty);

    public int Line { get; init; } = 1;

    public int Col { get; init; } = 1;
    
    public Cursor(string text)
    : this(text.EnumerateRunes().ToList(), 0)
    {}

    public Cursor Next()
    {
        if (!More) 
            return this;

        if (Current.ToString() == "\n")
            return new Cursor(Source, Offset + 1) { Line = Line + 1, Col = 1 };

        if (Current.ToString() != "\r")
            return new Cursor(Source, Offset + Current.Utf16SequenceLength) { Line = Line, Col = Col + 1 };

        var crlf = Offset + 1 < Source.Count && Source[Offset + 1].ToString() == "\n";
        return new Cursor(Source, Offset + (crlf ? 2 : 1)) { Line = Line + 1, Col = 1 };
    }
}

internal sealed partial record TokenCursor
{
    public TokenCursor Next() => More ? new TokenCursor(this, Offset + 1) : this;    

    public int Line => Current.Start.Line;

    public int Col => Current.Start.Col;
}

internal static class RuneExtensions
{
    public static bool Is(this Rune rune, string comparand) =>
        rune.ToString() == comparand;

    public static bool IsNot(this Rune rune, string comparand) =>
        rune.ToString() != comparand;
}