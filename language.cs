using static TokenParsers;

public sealed partial class Arafel
{
    public static Func<TokenTree?, TokenCursor, (TokenTree?, TokenCursor)> programHook = (result, cursor) => (result, cursor);
    
    public static (TokenTree? Result, TokenCursor Next) program(TokenCursor cursor)
    {
        if (!program_0(ref cursor, out var result)) return (null, cursor);
        if (result is not null && string.IsNullOrEmpty(result.Production)) result.Production = "program";
        return programHook(result, cursor);
    }
    
    static bool program_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = assignment(cursor);
        return result is not null;
    }
    
    static bool program_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = typeDef(cursor);
        return result is not null;
    }
    
    static bool program_4(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = customop(cursor);
        return result is not null;
    }
    
    static bool program_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = function(cursor);
        return result is not null;
    }
    
    static bool program_6(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool program_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (program_2(ref cursor, out result)) return true;
        if (program_3(ref cursor, out result)) return true;
        if (program_4(ref cursor, out result)) return true;
        if (program_5(ref cursor, out result)) return true;
        if (program_6(ref cursor, out result)) return true;
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
    
    static bool quoted_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "let")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool assignment_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = pattern(cursor);
        return result is not null;
    }
    
    static bool quoted_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "=")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool assignment_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool assignment_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_0(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!assignment_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!assignment_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool expr_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = function(cursor);
        return result is not null;
    }
    
    static bool expr_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (expr_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool expr_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op1(cursor);
        return result is not null;
    }
    
    static bool expr_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!expr_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!expr_3(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op1_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op2(cursor);
        return result is not null;
    }
    
    static bool op1_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP1", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op1_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op2(cursor);
        return result is not null;
    }
    
    static bool op1_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op1_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op1_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op1_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op1_3(ref cursor, out result))
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
        if (!op1_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op1_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op2_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op3(cursor);
        return result is not null;
    }
    
    static bool op2_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP2", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op2_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op3(cursor);
        return result is not null;
    }
    
    static bool op2_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op2_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op2_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op2_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op2_3(ref cursor, out result))
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
        if (!op2_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op2_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op3_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op4(cursor);
        return result is not null;
    }
    
    static bool op3_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP3", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op3_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op4(cursor);
        return result is not null;
    }
    
    static bool op3_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op3_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op3_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op3_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op3_3(ref cursor, out result))
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
        if (!op3_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op3_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op4_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op5(cursor);
        return result is not null;
    }
    
    static bool op4_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP4", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op4_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op5(cursor);
        return result is not null;
    }
    
    static bool op4_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op4_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op4_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op4_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op4_3(ref cursor, out result))
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
        if (!op4_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op4_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op5_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op6(cursor);
        return result is not null;
    }
    
    static bool op5_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP5", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op5_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op6(cursor);
        return result is not null;
    }
    
    static bool op5_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op5_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op5_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op5_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op5_3(ref cursor, out result))
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
        if (!op5_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op5_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op6_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op7(cursor);
        return result is not null;
    }
    
    static bool op6_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP6", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op6_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op7(cursor);
        return result is not null;
    }
    
    static bool op6_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op6_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op6_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op6_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op6_3(ref cursor, out result))
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
        if (!op6_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op6_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op7_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op8(cursor);
        return result is not null;
    }
    
    static bool op7_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP7", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op7_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op8(cursor);
        return result is not null;
    }
    
    static bool op7_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op7_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op7_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op7_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op7_3(ref cursor, out result))
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
        if (!op7_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op7_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op8_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op9(cursor);
        return result is not null;
    }
    
    static bool op8_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP8", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op8_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op9(cursor);
        return result is not null;
    }
    
    static bool op8_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op8_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op8_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op8_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op8_3(ref cursor, out result))
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
        if (!op8_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op8_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op9_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op10(cursor);
        return result is not null;
    }
    
    static bool op9_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP9", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op9_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op10(cursor);
        return result is not null;
    }
    
    static bool op9_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op9_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op9_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op9_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op9_3(ref cursor, out result))
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
        if (!op9_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op9_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op10_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op11(cursor);
        return result is not null;
    }
    
    static bool op10_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP10", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op10_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op11(cursor);
        return result is not null;
    }
    
    static bool op10_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op10_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op10_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op10_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op10_3(ref cursor, out result))
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
        if (!op10_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op10_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op11_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op12(cursor);
        return result is not null;
    }
    
    static bool op11_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP11", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op11_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = op12(cursor);
        return result is not null;
    }
    
    static bool op11_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op11_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op11_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op11_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op11_3(ref cursor, out result))
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
        if (!op11_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op11_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool op12_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = unary(cursor);
        return result is not null;
    }
    
    static bool op12_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("OP12", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool op12_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = unary(cursor);
        return result is not null;
    }
    
    static bool op12_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!op12_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op12_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool op12_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (op12_3(ref cursor, out result))
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
        if (!op12_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!op12_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool unary_2(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("PREFIX", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool unary_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (unary_2(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool unary_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = atom(cursor);
        return result is not null;
    }
    
    static bool unary_6(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is SuperToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool unary_7(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Context.Operators.Contains("POSTFIX", cursor.Current.Text))
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool unary_5(ref TokenCursor cursor, out TokenTree? result)
    {
        if (unary_6(ref cursor, out result)) return true;
        if (unary_7(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool unary_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (unary_5(ref cursor, out result))
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
        if (!unary_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!unary_4(ref cursor, out result)) { cursor = start; return false; }
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
        if (cursor.Current is LiteralToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool atom_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = ifthen(cursor);
        return result is not null;
    }
    
    static bool atom_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = exfix(cursor);
        return result is not null;
    }
    
    static bool atom_4(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = lambda(cursor);
        return result is not null;
    }
    
    static bool atom_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = parens(cursor);
        return result is not null;
    }
    
    static bool atom_6(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = call(cursor);
        return result is not null;
    }
    
    static bool atom_7(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool atom_8(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = list(cursor);
        return result is not null;
    }
    
    static bool atom_9(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = array(cursor);
        return result is not null;
    }
    
    static bool atom_10(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = match(cursor);
        return result is not null;
    }
    
    static bool atom_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (atom_1(ref cursor, out result)) return true;
        if (atom_2(ref cursor, out result)) return true;
        if (atom_3(ref cursor, out result)) return true;
        if (atom_4(ref cursor, out result)) return true;
        if (atom_5(ref cursor, out result)) return true;
        if (atom_6(ref cursor, out result)) return true;
        if (atom_7(ref cursor, out result)) return true;
        if (atom_8(ref cursor, out result)) return true;
        if (atom_9(ref cursor, out result)) return true;
        if (atom_10(ref cursor, out result)) return true;
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
    
    static bool quoted_2(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "if")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool ifthen_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool quoted_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "then")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool ifthen_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool quoted_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "else")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool ifthen_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool ifthen_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ifthen_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ifthen_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ifthen_3(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool quoted_5(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "(")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool parens_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool quoted_6(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == ",")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool parens_4(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool parens_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!parens_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool parens_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (parens_3(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool quoted_7(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == ")")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool parens_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!parens_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!parens_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_7(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool quoted_8(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "`")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool exfix_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is OperatorToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool exfix_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool exfix_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (exfix_5(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool exfix_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!exfix_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!exfix_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool exfix_7(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool exfix_8(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is OperatorToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool exfix_6(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!exfix_7(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!exfix_8(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool exfix_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (exfix_2(ref cursor, out result)) return true;
        if (exfix_6(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool exfix_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_8(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!exfix_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_8(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool call_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool call_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool call_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool call_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!call_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool call_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (call_4(ref cursor, out result))
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
        if (!call_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!call_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!call_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_7(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool quoted_9(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "[")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool list_6(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool list_9(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool list_8(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_9(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool list_7(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (list_8(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool list_5(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!list_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_7(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool quoted_10(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "..")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool list_11(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool list_10(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (list_11(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool list_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!list_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_10(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_10(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool list_13(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool list_16(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool list_15(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_16(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool list_14(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (list_15(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool list_12(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!list_13(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_14(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool list_2(ref TokenCursor cursor, out TokenTree? result)
    {
        if (list_3(ref cursor, out result)) return true;
        if (list_12(ref cursor, out result)) return true;
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
    
    static bool quoted_11(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "]")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool list_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_9(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!list_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_11(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool quoted_12(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "{")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool array_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool array_6(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool array_5(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!array_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool array_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (array_5(ref cursor, out result))
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
        if (!array_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!array_4(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool quoted_13(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "}")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool array_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_12(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!array_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_13(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool quoted_14(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "case")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool match_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool quoted_15(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "of")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool match_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = pattern(cursor);
        return result is not null;
    }
    
    static bool match_6(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is LiteralToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool quoted_16(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "-")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool match_8(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (quoted_16(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool match_9(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is NatToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool match_7(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!match_8(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!match_9(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool match_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (match_5(ref cursor, out result)) return true;
        if (match_6(ref cursor, out result)) return true;
        if (match_7(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool quoted_17(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "->")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool match_10(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool match_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!match_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_17(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!match_10(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool match_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (match_3(ref cursor, out result))
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
    
    static bool match_13(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool match_12(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_17(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!match_13(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool match_11(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (match_12(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool match_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_14(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!match_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_15(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!match_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!match_11(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool pattern_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = listPattern(cursor);
        return result is not null;
    }
    
    static bool pattern_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = tuplePattern(cursor);
        return result is not null;
    }
    
    static bool pattern_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = arrayPattern(cursor);
        return result is not null;
    }
    
    static bool pattern_4(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = ctorPattern(cursor);
        return result is not null;
    }
    
    static bool pattern_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (pattern_1(ref cursor, out result)) return true;
        if (pattern_2(ref cursor, out result)) return true;
        if (pattern_3(ref cursor, out result)) return true;
        if (pattern_4(ref cursor, out result)) return true;
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
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool ctorPattern_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = ids(cursor);
        return result is not null;
    }
    
    static bool ctorPattern_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (ctorPattern_3(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool ctorPattern_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!ctorPattern_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ctorPattern_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool ids_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool ids_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool ids_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ids_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool ids_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (ids_3(ref cursor, out result))
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
        if (!quoted_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ids_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!ids_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_7(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool listPattern_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool quoted_18(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == ":")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool listPattern_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!listPattern_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_18(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool listPattern_6(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool listPattern_7(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_9(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_11(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool listPattern_5(ref TokenCursor cursor, out TokenTree? result)
    {
        if (listPattern_6(ref cursor, out result)) return true;
        if (listPattern_7(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool listPattern_1(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!listPattern_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!listPattern_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool listPattern_8(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_9(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_11(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool listPattern_0(ref TokenCursor cursor, out TokenTree? result)
    {
        if (listPattern_1(ref cursor, out result)) return true;
        if (listPattern_8(ref cursor, out result)) return true;
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
    
    static bool tuplePattern_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool tuplePattern_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool tuplePattern_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!tuplePattern_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool tuplePattern_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (tuplePattern_3(ref cursor, out result))
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
        if (!tuplePattern_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!tuplePattern_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool arrayPattern_5(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool arrayPattern_10(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool arrayPattern_9(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!arrayPattern_10(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_8(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (arrayPattern_9(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_7(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!arrayPattern_8(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_10(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_6(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (arrayPattern_7(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool arrayPattern_4(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!arrayPattern_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!arrayPattern_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_12(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool arrayPattern_11(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_10(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!arrayPattern_12(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (arrayPattern_4(ref cursor, out result)) return true;
        if (arrayPattern_11(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool arrayPattern_15(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool arrayPattern_14(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!arrayPattern_15(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool arrayPattern_13(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (arrayPattern_14(ref cursor, out result))
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
        if (!arrayPattern_13(ref cursor, out result)) { cursor = start; return false; }
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
        if (!quoted_12(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!arrayPattern_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_13(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool function_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool function_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = ids(cursor);
        return result is not null;
    }
    
    static bool function_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool function_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!function_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!function_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!function_3(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool lambda_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = ids(cursor);
        return result is not null;
    }
    
    static bool lambda_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool lambda_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!lambda_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!lambda_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool quoted_19(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "op")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool customop_2(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = infix(cursor);
        return result is not null;
    }
    
    static bool customop_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = prefix(cursor);
        return result is not null;
    }
    
    static bool customop_4(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = postfix(cursor);
        return result is not null;
    }
    
    static bool customop_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (customop_2(ref cursor, out result)) return true;
        if (customop_3(ref cursor, out result)) return true;
        if (customop_4(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool customop_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = expr(cursor);
        return result is not null;
    }
    
    static bool customop_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_19(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!customop_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!customop_5(ref cursor, out result)) { cursor = start; return false; }
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
        if (quoted_16(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool infix_2(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is NatToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool infix_3(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool infix_5(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is OperatorToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool infix_6(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool infix_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (infix_5(ref cursor, out result)) return true;
        if (infix_6(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool infix_7(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool infix_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!infix_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!infix_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!infix_3(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!infix_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!infix_7(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool prefix_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is OperatorToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool prefix_2(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool prefix_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!prefix_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!prefix_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool postfix_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool postfix_2(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is OperatorToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool postfix_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!postfix_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!postfix_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool quoted_20(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "type")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool typeDef_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool typeDef_3(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = ids(cursor);
        return result is not null;
    }
    
    static bool typeDef_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (typeDef_3(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool typeDef_5(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = unionDef(cursor);
        return result is not null;
    }
    
    static bool typeDef_6(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = tupleDef(cursor);
        return result is not null;
    }
    
    static bool typeDef_4(ref TokenCursor cursor, out TokenTree? result)
    {
        if (typeDef_5(ref cursor, out result)) return true;
        if (typeDef_6(ref cursor, out result)) return true;
        result = null;
        return false;
    }
    
    static bool typeDef_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_20(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeDef_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeDef_2(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeDef_4(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool unionDef_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = typeName(cursor);
        return result is not null;
    }
    
    static bool quoted_21(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "|")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool unionDef_4(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = typeName(cursor);
        return result is not null;
    }
    
    static bool unionDef_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_21(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!unionDef_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool unionDef_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (unionDef_3(ref cursor, out result))
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
        if (!unionDef_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!unionDef_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool tupleDef_1(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = typeName(cursor);
        return result is not null;
    }
    
    static bool quoted_22(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current.Text == "*")
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool tupleDef_4(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = typeName(cursor);
        return result is not null;
    }
    
    static bool tupleDef_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_22(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!tupleDef_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool tupleDef_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (tupleDef_3(ref cursor, out result))
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
        if (!tupleDef_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!tupleDef_2(ref cursor, out result)) { cursor = start; return false; }
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
    
    static bool typeName_1(ref TokenCursor cursor, out TokenTree? result)
    {
        if (cursor.Current is IdToken)
        {
            result = new TokenTree(cursor);
            cursor = cursor.Next();
            return true;
        }
        
        result = null;
        return false;
    }
    
    static bool typeName_4(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = typeName(cursor);
        return result is not null;
    }
    
    static bool typeName_7(ref TokenCursor cursor, out TokenTree? result)
    {
        (result, cursor) = typeName(cursor);
        return result is not null;
    }
    
    static bool typeName_6(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_6(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeName_7(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool typeName_5(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        
        while (typeName_6(ref cursor, out result))
        {
            Append(list, result);
        }
        
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool typeName_3(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!quoted_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeName_4(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeName_5(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!quoted_7(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (list.Count == 1) result = list[0];
        else result = new TokenTree(start, cursor, list);
        return true;
    }
    
    static bool typeName_2(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        if (typeName_3(ref cursor, out result)) return true;
        result = new TokenTree(start, start);
        return true;
    }
    
    static bool typeName_0(ref TokenCursor cursor, out TokenTree? result)
    {
        var start = cursor;
        var list = new List<TokenTree>();
        if (!typeName_1(ref cursor, out result)) { cursor = start; return false; }
        Append(list, result);
        if (!typeName_2(ref cursor, out result)) { cursor = start; return false; }
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
}
