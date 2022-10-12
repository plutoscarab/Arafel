using System.Text;

internal record Cursor(string SourceText, int Offset, int Line, int Col)
{
    public static Cursor Empty = new Cursor(string.Empty);
    
    public Cursor(string text)
    : this(text, 0, 1, 1)
    {}

    public bool More => Offset < SourceText.Length;

    public Rune Current => 
        More && Rune.TryGetRuneAt(SourceText, Offset, out var rune) ? rune : new Rune(0);

    public Cursor Next()
    {
        if (!More) 
            return this;

        if (Current.ToString() == "\n")
            return new Cursor(SourceText, Offset + 1, Line + 1, 1);

        if (Current.ToString() != "\r")
            return new Cursor(SourceText, Offset + Current.Utf16SequenceLength, Line, Col + 1);

        var crlf = Offset + 1 < SourceText.Length && SourceText[Offset + 1] == '\n';
        return new Cursor(SourceText, Offset + (crlf ? 2 : 1), Line + 1, 1);
    }

    public static bool operator >(Cursor a, Cursor b) => a.Offset > b.Offset;
    public static bool operator <(Cursor a, Cursor b) => a.Offset < b.Offset;
    public static int operator -(Cursor a, Cursor b) => a.Offset - b.Offset;
}

internal static class RuneExtensions
{
    public static bool Is(this Rune rune, string comparand) =>
        rune.ToString() == comparand;

    public static bool IsNot(this Rune rune, string comparand) =>
        rune.ToString() != comparand;
}