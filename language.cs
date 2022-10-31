namespace Plutoscarab.Arafel;

public sealed partial class Arafel
{
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> programHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) program(TokenCursor cursor)
    {
        if (!program_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "program";
        return programHook(result, cursor);
    }
    
    static bool program_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Production(assignment, ref cursor, out result)) return true;
        if (Production(typeDef, ref cursor, out result)) return true;
        if (Production(customop, ref cursor, out result)) return true;
        if (Production(function, ref cursor, out result)) return true;
        if (Production(expr, ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool program_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (program_1(ref cursor, out result))
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
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> assignmentHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) assignment(TokenCursor cursor)
    {
        if (!assignment_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "assignment";
        return assignmentHook(result, cursor);
    }
    
    static bool assignment_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("let", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(pattern, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("=", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> exprHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) expr(TokenCursor cursor)
    {
        if (!expr_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "expr";
        return exprHook(result, cursor);
    }
    
    static bool expr_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (Production(function, ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool expr_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!expr_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op1, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op1Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op1(TokenCursor cursor)
    {
        if (!op1_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op1";
        return op1Hook(result, cursor);
    }
    
    static bool op1_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP1", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op2, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op1_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op1_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op1_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op2, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op1_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op2Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op2(TokenCursor cursor)
    {
        if (!op2_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op2";
        return op2Hook(result, cursor);
    }
    
    static bool op2_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP2", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op3, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op2_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op2_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op2_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op3, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op2_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op3Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op3(TokenCursor cursor)
    {
        if (!op3_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op3";
        return op3Hook(result, cursor);
    }
    
    static bool op3_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP3", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op4, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op3_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op3_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op3_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op4, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op3_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op4Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op4(TokenCursor cursor)
    {
        if (!op4_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op4";
        return op4Hook(result, cursor);
    }
    
    static bool op4_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP4", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op5, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op4_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op4_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op4_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op5, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op4_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op5Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op5(TokenCursor cursor)
    {
        if (!op5_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op5";
        return op5Hook(result, cursor);
    }
    
    static bool op5_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP5", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op6, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op5_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op5_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op5_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op6, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op5_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op6Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op6(TokenCursor cursor)
    {
        if (!op6_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op6";
        return op6Hook(result, cursor);
    }
    
    static bool op6_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP6", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op7, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op6_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op6_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op6_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op7, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op6_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op7Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op7(TokenCursor cursor)
    {
        if (!op7_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op7";
        return op7Hook(result, cursor);
    }
    
    static bool op7_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP7", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op8, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op7_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op7_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op7_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op8, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op7_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op8Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op8(TokenCursor cursor)
    {
        if (!op8_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op8";
        return op8Hook(result, cursor);
    }
    
    static bool op8_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP8", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op9, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op8_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op8_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op8_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op9, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op8_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op9Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op9(TokenCursor cursor)
    {
        if (!op9_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op9";
        return op9Hook(result, cursor);
    }
    
    static bool op9_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP9", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op10, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op9_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op9_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op9_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op10, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op9_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op10Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op10(TokenCursor cursor)
    {
        if (!op10_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op10";
        return op10Hook(result, cursor);
    }
    
    static bool op10_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP10", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op11, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op10_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op10_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op10_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op11, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op10_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op11Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op11(TokenCursor cursor)
    {
        if (!op11_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op11";
        return op11Hook(result, cursor);
    }
    
    static bool op11_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP11", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(op12, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op11_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op11_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op11_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(op12, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op11_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> op12Hook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) op12(TokenCursor cursor)
    {
        if (!op12_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "op12";
        return op12Hook(result, cursor);
    }
    
    static bool op12_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Operator("OP12", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(unary, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op12_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op12_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op12_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(unary, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op12_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> unaryHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) unary(TokenCursor cursor)
    {
        if (!unary_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "unary";
        return unaryHook(result, cursor);
    }
    
    static bool unary_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (Operator("PREFIX", ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool unary_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (TokenType<SuperToken>(ref cursor, out result)) return true;
        if (Operator("POSTFIX", ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool unary_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (unary_3(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool unary_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!unary_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(atom, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!unary_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool atom_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (TokenType<LiteralToken>(ref cursor, out result)) return true;
        if (Production(ifthen, ref cursor, out result)) return true;
        if (Production(exfix, ref cursor, out result)) return true;
        if (Production(lambda, ref cursor, out result)) return true;
        if (Production(parens, ref cursor, out result)) return true;
        if (Production(call, ref cursor, out result)) return true;
        if (Production(match, ref cursor, out result)) return true;
        if (TokenType<IdToken>(ref cursor, out result)) return true;
        if (Production(list, ref cursor, out result)) return true;
        if (Production(array, ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> ifthenHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) ifthen(TokenCursor cursor)
    {
        if (!ifthen_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "ifthen";
        return ifthenHook(result, cursor);
    }
    
    static bool ifthen_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("if", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("then", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("else", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> parensHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) parens(TokenCursor cursor)
    {
        if (!parens_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "parens";
        return parensHook(result, cursor);
    }
    
    static bool parens_2(ref TokenCursor cursor, out TokenTree? result)
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
    
    static bool parens_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (parens_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool parens_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("(", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!parens_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(")", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> exfixHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) exfix(TokenCursor cursor)
    {
        if (!exfix_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "exfix";
        return exfixHook(result, cursor);
    }
    
    static bool exfix_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (Production(expr, ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool exfix_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<OperatorToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!exfix_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool exfix_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<OperatorToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool exfix_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (exfix_2(ref cursor, out result)) return true;
        if (exfix_4(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool exfix_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("`", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!exfix_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("`", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> callHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) call(TokenCursor cursor)
    {
        if (!call_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "call";
        return callHook(result, cursor);
    }
    
    static bool call_2(ref TokenCursor cursor, out TokenTree? result)
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
    
    static bool call_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (call_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool call_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("(", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!call_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(")", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> listHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) list(TokenCursor cursor)
    {
        if (!list_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "list";
        return listHook(result, cursor);
    }
    
    static bool list_7(ref TokenCursor cursor, out TokenTree? result)
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
    
    static bool list_6(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (list_7(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool list_5(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool list_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (list_5(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool list_8(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (Production(expr, ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool list_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!list_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("..", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_8(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool list_11(ref TokenCursor cursor, out TokenTree? result)
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
    
    static bool list_10(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (list_11(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool list_9(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_10(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool list_2(ref TokenCursor cursor, out TokenTree? result)
    {
        if (list_3(ref cursor, out result)) return true;
        if (list_9(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool list_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (list_2(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool list_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("[", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("]", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> arrayHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) array(TokenCursor cursor)
    {
        if (!array_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "array";
        return arrayHook(result, cursor);
    }
    
    static bool array_4(ref TokenCursor cursor, out TokenTree? result)
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
    
    static bool array_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (array_4(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool array_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!array_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool array_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (array_2(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool array_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("{", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!array_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("}", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> matchHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) match(TokenCursor cursor)
    {
        if (!match_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "match";
        return matchHook(result, cursor);
    }
    
    static bool match_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Production(matchCase, ref cursor, out result)) return true;
        if (Production(ifCase, ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool match_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("case", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!match_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> matchCaseHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) matchCase(TokenCursor cursor)
    {
        if (!matchCase_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "matchCase";
        return matchCaseHook(result, cursor);
    }
    
    static bool matchCase_5(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (Literal("-", ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool matchCase_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!matchCase_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<NatToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool matchCase_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Production(pattern, ref cursor, out result)) return true;
        if (TokenType<LiteralToken>(ref cursor, out result)) return true;
        if (matchCase_4(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool matchCase_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!matchCase_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("->", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool matchCase_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (matchCase_2(ref cursor, out result))
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
    
    static bool matchCase_7(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("else", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool matchCase_6(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (matchCase_7(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool matchCase_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("of", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!matchCase_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!matchCase_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> ifCaseHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) ifCase(TokenCursor cursor)
    {
        if (!ifCase_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "ifCase";
        return ifCaseHook(result, cursor);
    }
    
    static bool ifCase_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("->", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool ifCase_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (ifCase_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool ifCase_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("of", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ifCase_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("else", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool pattern_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Production(listPattern, ref cursor, out result)) return true;
        if (Production(tuplePattern, ref cursor, out result)) return true;
        if (Production(arrayPattern, ref cursor, out result)) return true;
        if (Production(ctorPattern, ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> ctorPatternHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) ctorPattern(TokenCursor cursor)
    {
        if (!ctorPattern_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "ctorPattern";
        return ctorPatternHook(result, cursor);
    }
    
    static bool ctorPattern_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (Production(ids, ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool ctorPattern_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ctorPattern_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> idsHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) ids(TokenCursor cursor)
    {
        if (!ids_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "ids";
        return idsHook(result, cursor);
    }
    
    static bool ids_2(ref TokenCursor cursor, out TokenTree? result)
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
    
    static bool ids_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (ids_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool ids_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("(", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ids_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(")", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> listPatternHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) listPattern(TokenCursor cursor)
    {
        if (!listPattern_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "listPattern";
        return listPatternHook(result, cursor);
    }
    
    static bool listPattern_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(":", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool listPattern_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (listPattern_3(ref cursor, out result))
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
    
    static bool listPattern_5(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("[", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("]", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool listPattern_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (TokenType<IdToken>(ref cursor, out result)) return true;
        if (listPattern_5(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool listPattern_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!listPattern_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!listPattern_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool listPattern_6(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("[", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("]", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool listPattern_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (listPattern_1(ref cursor, out result)) return true;
        if (listPattern_6(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> tuplePatternHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) tuplePattern(TokenCursor cursor)
    {
        if (!tuplePattern_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "tuplePattern";
        return tuplePatternHook(result, cursor);
    }
    
    static bool tuplePattern_2(ref TokenCursor cursor, out TokenTree? result)
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
    
    static bool tuplePattern_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (tuplePattern_2(ref cursor, out result))
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
    
    static bool tuplePattern_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!tuplePattern_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> arrayPatternHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) arrayPattern(TokenCursor cursor)
    {
        if (!arrayPattern_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "arrayPattern";
        return arrayPatternHook(result, cursor);
    }
    
    static bool arrayPattern_8(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(",", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_7(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (arrayPattern_8(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_6(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal(",", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!arrayPattern_7(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("..", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_5(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (arrayPattern_6(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool arrayPattern_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!arrayPattern_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_9(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("..", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(",", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (arrayPattern_4(ref cursor, out result)) return true;
        if (arrayPattern_9(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool arrayPattern_11(ref TokenCursor cursor, out TokenTree? result)
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
    
    static bool arrayPattern_10(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (arrayPattern_11(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!arrayPattern_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!arrayPattern_10(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (arrayPattern_2(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool arrayPattern_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("{", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!arrayPattern_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("}", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> functionHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) function(TokenCursor cursor)
    {
        if (!function_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "function";
        return functionHook(result, cursor);
    }
    
    static bool function_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(ids, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("=", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool lambda_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(ids, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("=", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> customopHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) customop(TokenCursor cursor)
    {
        if (!customop_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "customop";
        return customopHook(result, cursor);
    }
    
    static bool customop_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Production(infix, ref cursor, out result)) return true;
        if (Production(prefix, ref cursor, out result)) return true;
        if (Production(postfix, ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool customop_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("op", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!customop_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("=", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(expr, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> infixHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) infix(TokenCursor cursor)
    {
        if (!infix_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "infix";
        return infixHook(result, cursor);
    }
    
    static bool infix_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (Literal("-", ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool infix_2(ref TokenCursor cursor, out TokenTree? result)
    {
        if (TokenType<OperatorToken>(ref cursor, out result)) return true;
        if (TokenType<IdToken>(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool infix_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!infix_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<NatToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!infix_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> prefixHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) prefix(TokenCursor cursor)
    {
        if (!prefix_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "prefix";
        return prefixHook(result, cursor);
    }
    
    static bool prefix_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<OperatorToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> postfixHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) postfix(TokenCursor cursor)
    {
        if (!postfix_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "postfix";
        return postfixHook(result, cursor);
    }
    
    static bool postfix_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<OperatorToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> typeDefHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) typeDef(TokenCursor cursor)
    {
        if (!typeDef_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "typeDef";
        return typeDefHook(result, cursor);
    }
    
    static bool typeDef_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (Production(ids, ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool typeDef_2(ref TokenCursor cursor, out TokenTree? result)
    {
        if (Production(unionDef, ref cursor, out result)) return true;
        if (Production(tupleDef, ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool typeDef_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("type", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeDef_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal("=", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeDef_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> unionDefHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) unionDef(TokenCursor cursor)
    {
        if (!unionDef_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "unionDef";
        return unionDefHook(result, cursor);
    }
    
    static bool unionDef_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("|", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(typeName, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool unionDef_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (unionDef_2(ref cursor, out result))
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
    
    static bool unionDef_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(typeName, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!unionDef_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> tupleDefHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) tupleDef(TokenCursor cursor)
    {
        if (!tupleDef_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "tupleDef";
        return tupleDefHook(result, cursor);
    }
    
    static bool tupleDef_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("*", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(typeName, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool tupleDef_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (tupleDef_2(ref cursor, out result))
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
    
    static bool tupleDef_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Production(typeName, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!tupleDef_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> typeNameHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) typeName(TokenCursor cursor)
    {
        if (!typeName_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "typeName";
        return typeNameHook(result, cursor);
    }
    
    static bool typeName_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal(",", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(typeName, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool typeName_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (typeName_4(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool typeName_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!Literal("(", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Production(typeName, ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeName_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!Literal(")", ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool typeName_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (typeName_2(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool typeName_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!TokenType<IdToken>(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeName_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
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
