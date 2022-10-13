
using System.Text;

internal sealed class Program
{
    public static void Main()
    {
        Console.OutputEncoding = Encoding.Unicode;
        var grammar = new Grammar("grammar.txt");
        var text = File.ReadAllText("sample.af");
        var tokens = Lexer.Tokenize(text).ToList();
        var cursor = new TokenCursor(tokens);
        var (result, next) = grammar.Program(cursor);
        if (result is null) throw new NotImplementedException();
        if (next.More) throw new NotImplementedException();

        foreach (var tree in result.Children)
        {
            Console.WriteLine();
            tree.Dump(Console.Out);
        }
    }
}