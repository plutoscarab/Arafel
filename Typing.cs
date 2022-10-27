using System.Text;

namespace Plutoscarab.Arafel;

public sealed class TypeVariable
{
    private static int nextId = 0;

    public readonly int Id = ++nextId;
}

public abstract record TypeExpr(int Precedence)
{
    public static TypeExpr Make(Random rand)
    {
        var typevars = new[] { new TypeVarExpr(), new TypeVarExpr(), new TypeVarExpr() };
        return Make(rand, typevars);
    }

    private static string MakeName(Random rand)
    {
        var s = new StringBuilder();
        for (var i = 0; i < 4; i++)
            s.Append((char)(97 + rand.Next(26)));
        s[0] = char.ToUpperInvariant(s[0]);
        return s.ToString();
    }

    public static TypeExpr Make(Random rand, TypeExpr[] typevars)
    {
        switch (rand.Next(8))
        {
            case 0: return new PrimTypeExpr(new[]{"Int","Char","Double","Str"}[rand.Next(4)]);
            case 1: return typevars.Length > 0 ? typevars[rand.Next(typevars.Length)] : Make(rand, typevars);
            case 2: return new ListTypeExpr(Make(rand, typevars));
            case 3: return new ArrayTypeExpr(Make(rand, typevars));
            case 4: return new SumTypeExpr(new TypeFuncExpr(MakeName(rand), typevars.Take(rand.Next(4)).ToArray()), new TypeFuncExpr(MakeName(rand), typevars.Take(rand.Next(4)).ToArray()));
            case 5: return new ProductTypeExpr(Make(rand, typevars), Make(rand, typevars));
            case 6: return new FuncTypeExpr(Make(rand, typevars), Make(rand, typevars));
            case 7: return new TypeFuncExpr(MakeName(rand), typevars.Take(rand.Next(4)).ToArray());
        }

        throw new NotImplementedException();
    }

    public abstract string ToStr();

    public override string ToString() => ToStr();
}

public sealed record PrimTypeExpr(string Name) : TypeExpr(10)
{
    public override string ToStr() => $"{Name}";
}

public sealed record TypeVarExpr(TypeVariable TypeVar) : TypeExpr(10)
{
    public TypeVarExpr() : this(new TypeVariable()) { }
    public override string ToStr() => $"var{TypeVar.Id}";
}

public abstract record BinaryTypeExpr(TypeExpr Left, TypeExpr Right, string Operator, int Precedence) : TypeExpr(Precedence)
{
    public override string ToStr()
    {
        var left = Left.ToStr();
        if (Left.Precedence < Precedence) left = "(" + left + ")";
        var right = Right.ToStr();
        if (Right.Precedence < Precedence) right = "(" + right + ")";
        return $"{left} {Operator} {right}";
    }

    public override string ToString() => ToStr();
}

public sealed record SumTypeExpr(TypeFuncExpr Left, TypeFuncExpr Right) : TypeExpr(2)
{
    public override string ToStr()
        {
            var left = Left.ToStr();
            if (Left.Precedence < Precedence) left = "(" + left + ")";
            var right = Right.ToStr();
            if (Right.Precedence < Precedence) right = "(" + right + ")";
            return $"{left} | {right}";
        }
}

public sealed record ProductTypeExpr(TypeExpr Left, TypeExpr Right) : BinaryTypeExpr(Left, Right, "*", 3);

public sealed record FuncTypeExpr(TypeExpr Left, TypeExpr Right) : BinaryTypeExpr(Left, Right, "->", 1);

public sealed record ListTypeExpr(TypeExpr Inner) : TypeExpr(10)
{
    public override string ToStr() => $"[{Inner.ToStr()}]";
}

public sealed record ArrayTypeExpr(TypeExpr Inner) : TypeExpr(10)
{
    public override string ToStr() => "{" + Inner.ToStr() + "}";
}

public sealed record TypeFuncExpr(string Name, params TypeExpr[] Params) : TypeExpr(10)
{
    public override string ToStr() => Params.Length == 0 ? Name : Name + "(" + string.Join(", ", Params.Select(p => p.ToStr())) + ")";
}