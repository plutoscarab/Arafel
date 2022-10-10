internal record UnknownNode(string Expected, Tursor Start, int Length)
: Node(Start, Length)
{
    public override object? NodeValue => $"{Expected} Line {Start.Current.Start.Line} Col {Start.Current.Start.Col}";
}