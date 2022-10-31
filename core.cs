namespace Plutoscarab.Arafel;

public sealed partial class Core
{
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> typeHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) type(TokenCursor cursor)
    {
        if (!type_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "type";
        return typeHook(result, cursor);
    }
    
    static bool type_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Literal("∀", ref cursor, out result)) return true;
        if (Literal("forall", ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool type_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (TokenType<IdToken>(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Any())
        {
            if (list.Count == 1) result = list[0];
            else result = new TokenTree(start, cursor, list);
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool type_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!type_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!type_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(",", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool type_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (type_2(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool type_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!type_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(monoType, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> monoTypeHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) monoType(TokenCursor cursor)
    {
        if (!monoType_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "monoType";
        return monoTypeHook(result, cursor);
    }
    
    static bool monoType_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Literal("→", ref cursor, out result)) return true;
        if (Literal("->", ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool monoType_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!monoType_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(productType, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool monoType_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (monoType_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool monoType_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(productType, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!monoType_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> productTypeHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) productType(TokenCursor cursor)
    {
        if (!productType_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "productType";
        return productTypeHook(result, cursor);
    }
    
    static bool productType_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Literal("×", ref cursor, out result)) return true;
        if (Literal("*", ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool productType_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!productType_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(typeAtom, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool productType_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (productType_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool productType_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(typeAtom, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!productType_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> typeAtomHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) typeAtom(TokenCursor cursor)
    {
        if (!typeAtom_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "typeAtom";
        return typeAtomHook(result, cursor);
    }
    
    static bool typeAtom_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (Production(monoType, ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool typeAtom_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeAtom_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool typeAtom_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("(", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(monoType, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(")", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool typeAtom_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (typeAtom_1(ref cursor, out result)) return true;
        if (typeAtom_3(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> exprHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) expr(TokenCursor cursor)
    {
        if (!expr_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "expr";
        return exprHook(result, cursor);
    }
    
    static bool expr_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal(",", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool expr_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (expr_4(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool expr_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("(", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!expr_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(")", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool expr_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (expr_2(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool expr_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(atom, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!expr_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> atomHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) atom(TokenCursor cursor)
    {
        if (!atom_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "atom";
        return atomHook(result, cursor);
    }
    
    static bool atom_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("`", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<OperatorToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("`", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool atom_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("(", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(")", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool atom_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("let", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(typedVar, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("=", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("in", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool atom_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (TokenType<IdToken>(ref cursor, out result)) return true;
        if (TokenType<LiteralToken>(ref cursor, out result)) return true;
        if (atom_1(ref cursor, out result)) return true;
        if (atom_2(ref cursor, out result)) return true;
        if (Production(lambda, ref cursor, out result)) return true;
        if (atom_3(ref cursor, out result)) return true;
        if (Production(case_, ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> typedVarHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) typedVar(TokenCursor cursor)
    {
        if (!typedVar_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "typedVar";
        return typedVarHook(result, cursor);
    }
    
    static bool typedVar_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(":", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(type, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> lambdaHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) lambda(TokenCursor cursor)
    {
        if (!lambda_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "lambda";
        return lambdaHook(result, cursor);
    }
    
    static bool lambda_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal(",", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(typedVar, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool lambda_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (lambda_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool lambda_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("(", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(typedVar, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!lambda_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(")", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("=", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> case_Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) case_(TokenCursor cursor)
    {
        if (!case__0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "case_";
        return case_Hook(result, cursor);
    }
    
    static bool case__3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Literal("→", ref cursor, out result)) return true;
        if (Literal("->", ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool case__2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(pattern, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!case__3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool case__1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (case__2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Any())
        {
            if (list.Count == 1) result = list[0];
            else result = new TokenTree(start, cursor, list);
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool case__0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("case", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("of", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!case__1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> patternHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) pattern(TokenCursor cursor)
    {
        if (!pattern_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "pattern";
        return patternHook(result, cursor);
    }
    
    static bool pattern_5(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal(",", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool pattern_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (pattern_5(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool pattern_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("(", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!pattern_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(")", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool pattern_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (pattern_3(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool pattern_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!pattern_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool pattern_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (TokenType<LiteralToken>(ref cursor, out result)) return true;
        if (pattern_1(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static void Append(List<TokenTree> list, TokenTree? result)
    {
        if (result is not null && result.Next > result.Start)
            if (result.Children.Any() && string.IsNullOrEmpty(result.Production))
                list.AddRange(result.Children);
            else
                list.Add(result);
    }
    
    static bool Literal(string literal, ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == literal)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
    
        result = null;
        return false;
    }
    
    static bool Production(TokenParser parser, ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = parser(cursor);
        return result is not null;
    }
    
    static bool TokenType<T>(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is T)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
    
        result = null;
        return false;
    }
    
    static bool Operator(string op, ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains(op, cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
    
        result = null;
        return false;
    }
}
