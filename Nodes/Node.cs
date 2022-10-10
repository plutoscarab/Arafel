internal record Node(Tursor Start, int Length)
{
    public virtual object? NodeValue => null;

    public virtual IEnumerable<Node> Children()
    {
        yield break;
    }
}