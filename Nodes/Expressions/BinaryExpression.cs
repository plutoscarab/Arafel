internal sealed record BinaryExpression(ExpressionNode Left, Token Op, ExpressionNode Right)
: ExpressionNode(Left.Start, Left.Length + 1 + Right.Length)
{
    public override object? NodeValue => Op.Text;

    public override IEnumerable<Node> Children()
    {
        yield return Left;
        yield return Right;
    }
}