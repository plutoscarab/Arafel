
using System.Text;

internal sealed class Program
{
    public static void Main()
    {
        Console.OutputEncoding = Encoding.Unicode;

        for (var cp = 0; cp < 0x110000; cp++)
        {
            if (cp >= 0xD800 && cp < 0xE000)
                continue;

            var s = char.ConvertFromUtf32(cp);
            var cat = char.GetUnicodeCategory(s, 0);

            if (cat == System.Globalization.UnicodeCategory.DecimalDigitNumber && char.GetNumericValue(s, 0) == 0)
            {
                Console.WriteLine($"{s}-{char.ConvertFromUtf32(cp + 9)} #x{cp:X}-#x{cp+9:X}");
            }
        }

        var text = File.ReadAllText("sample.af");
        var tokens = Lexer.Tokenize(text).ToList();
        var trees = Arafel.Parse(tokens).ToList();

        foreach (var tree in trees)
        {
            Console.WriteLine();
            Dump(tree);
        }
    }

    private static void Dump(TokenTree tree)
    {
        Dump(TreeLabel.Root, tree, Console.Out, string.Empty);
    }

    private static void Dump(TreeLabel label, TokenTree tree, TextWriter writer, string indent, bool last = true)
    {
        var branch = last ? "└" : "├";
        var token = tree.Parts.Any() ? "" : string.Join(" ", tree.Tokens().Select(t => t.Text));
        var labelStr = (int)label < 0 ? "[" + (-(int)label-1) + "]" : label.ToString();
        Console.WriteLine($"{indent}{branch}─{labelStr}: {tree.Kind} {token}");
        indent += last ? "  " : "│ ";
        
        foreach (var (child, i) in tree.Parts.Select((kvp, i) => (kvp, i)).OrderByDescending(p => (int)p.kvp.Key))
        {
            Dump(child.Key, child.Value, writer, indent, i == 0);
        }
    }
}