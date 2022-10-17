public sealed record Context(Operators Operators)
{
    public Context WithOperator(string production, string op) =>
        new Context(Operators.WithOperator(production, op));
}