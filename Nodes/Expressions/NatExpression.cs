using System.Numerics;

internal record NatExpression(BigInteger Value, Tursor Start, int Length) 
: ExpressionNode(Start, Length)
{
    public override object? NodeValue => Value;
}