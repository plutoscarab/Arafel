internal sealed class Grammar
{
    public Grammar(string filename)
    {
        foreach (var line in File.ReadLines(filename).Where(line => !string.IsNullOrWhiteSpace(line)))
        {
            var parts = line.Split("::=");
            var production = parts[0];
            var expr = parts[1];
            
        }
    }
}