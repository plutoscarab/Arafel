using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public sealed partial class Operators
{
    private readonly Dictionary<string, HashSet<string>> lookup;

    public Operators()
    {
        lookup = new Dictionary<string, HashSet<string>>();
    }

    private Operators(Dictionary<string, HashSet<string>> lookup)
    {
        this.lookup = lookup;
    }

    public Operators(string filename)
    {
        lookup = File.ReadAllLines(filename)
            .Select(line => line.Split(' '))
            .ToDictionary(p => p[0], p => p.Skip(1).Where(q => q.Length > 0).ToHashSet());
    }

    public Operators WithOperator(string production, string op)
    {
        var dict = lookup.ToDictionary(p => p.Key, p => p.Value);
        dict[production].Add(op);
        return new Operators(dict);
    }

    public bool Contains(string production, string op)
    {
        return lookup.TryGetValue(production, out var list) && list.Contains(op);
    }
}
