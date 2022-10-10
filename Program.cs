
using System.Text;

internal sealed class Program
{
    public static void Main()
    {
        Console.OutputEncoding = Encoding.Unicode;
        var text = File.ReadAllText("sample.af");
        var tokens = Lexer.Tokenize(text).ToList();

        foreach (var node in Arafel.Parse(tokens))
        {
            Console.WriteLine();
            Dump(node);
        }
    }

    private static void Dump(TokenTree node)
    {
        Dump(TreeLabel.Root, node, Console.Out, string.Empty);
    }

    private static void Dump(TreeLabel label, TokenTree node, TextWriter writer, string indent, bool last = true)
    {
        var branch = last ? "└" : "├";
        var token = node.Token?.Current.Text ?? "";
        if (token.Length > 0) token = "'" + token + "'";
        var labelStr = (int)label < 0 ? "[" + (-(int)label-1) + "]" : label.ToString();
        Console.WriteLine($"{indent}{branch}─{labelStr}: {node.Kind} {token}");
        indent += last ? "  " : "│ ";
        
        foreach (var (child, i) in node.Parts.Select((kvp, i) => (kvp, i)).OrderByDescending(p => (int)p.kvp.Key))
        {
            Dump(child.Key, child.Value, writer, indent, i == 0);
        }
    }
}