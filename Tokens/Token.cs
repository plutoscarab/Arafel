internal abstract record Token(Cursor Start, int Length)
{
    public string Text => 
        Start.SourceText.Substring(Start.Offset, Length);

    public override string ToString() => 
        $"Line {Start.Line} Col {Start.Col} '{Text}'";

    public virtual Object? Value => null;
}
