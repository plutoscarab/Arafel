using System.Text;

internal partial record Token
{
    public int Length => Next.Offset - Start.Offset;

    public string Text => 
        Children.Any() ? "" : FullText();

    public string FullText()
    {
        var s = new StringBuilder();

        foreach (var t in Runes())
        {
            s.Append(t.ToString());
        }

        return s.ToString();
    }

    public string ChildText() =>
        Children.Any() ? string.Join("", Children.Select(t => t.ChildText())) : FullText();

    public override string ToString() =>
        Text;
}
