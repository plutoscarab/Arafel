internal sealed record BadExpression(string Reason, Tursor Start, int Length)
: ExpressionNode(Start, Length);