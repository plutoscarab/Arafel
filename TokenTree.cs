using System.Collections.Immutable;

enum TreeKind {
    Nat,
    SyntaxError,
    Token,
    Items,
    Operator,
    OpAndOperand,
    Association,
    Parens,
    Function,
    Call,
    Identifier,
    List,
    Item,
    Nop,
    Etc,
    Unary,
    Union,
    Association_,
    Assignment,
    Char,
    String,
    Case,
    Patterns,
    Pattern,
    ConstructorPattern,
}

enum TreeLabel {
    Root,
    List,
    Operand,
    Operator,
    First,
    Rest,
    Ignore,
    Identifier,
    Parameters,
    Arguments,
    Values,
    Item,
    Etc,
    Final,
    Constructors,
    Expression,
    TypeName,
    Patterns,
    Pattern
}

internal sealed record TokenTree(
    TreeKind Kind, 
    Tursor Start,
    Tursor Next,
    ImmutableDictionary<TreeLabel, TokenTree> Parts)
{
    public TreeLabel Label { get; private set; } = TreeLabel.Ignore;

    public TokenTree(TreeKind kind, Tursor tursor)
    : this(kind, tursor, tursor.Next(), ImmutableDictionary<TreeLabel, TokenTree>.Empty)
    { }

    public TokenTree(TreeKind kind, Tursor start, Tursor next)
    : this(kind, start, next, ImmutableDictionary<TreeLabel, TokenTree>.Empty)
    { }

    public TokenTree(TokenCursor tursor)
    : this(TreeKind.Token, new Tursor(tursor.Source, tursor.Offset))
    { }

    public TokenTree(TokenCursor start, TokenCursor next)
    : this(TreeKind.Token, new Tursor(start.Source, start.Offset), new Tursor(next.Source, next.Offset))
    { }

    public IEnumerable<Token> Tokens()
    {
        var t = Start;

        while (t.Offset < Next.Offset)
        {
            yield return t.Current;
            t = t.Next();
        }
    }

    private static Tursor RangeStart(IEnumerable<TokenTree> trees) =>
        trees.Any() 
            ? trees.Where(t => t.Start.Offset < t.Next.Offset).MinBy(t => t.Start.Offset)?.Start ?? trees.First().Start
            : new Tursor(new Token[0], 0);

    private static Tursor RangeNext(IEnumerable<TokenTree> trees) =>
        trees.Any()
            ? trees.Where(t => t.Start.Offset < t.Next.Offset).MaxBy(t => t.Next.Offset)?.Next ?? trees.First().Start
            : new Tursor(new Token[0], 0);

    public TokenTree(TreeKind kind, IEnumerable<TokenTree> trees)
    : this(kind, RangeStart(trees), RangeNext(trees), TreeDictionary(trees))
    { }

    public TokenTree(TreeKind kind, params (TreeLabel Label, TokenTree Tree)[] trees)
    : this(kind, RangeStart(trees.Select(t => t.Tree)), RangeNext(trees.Select(t => t.Tree)), trees.ToDictionary(t => t.Label, t => t.Tree).ToImmutableDictionary())
    { }

    public TokenTree Tag(TreeLabel label)
    {
        var tree = new TokenTree(this.Kind, this.Start, this.Next, this.Parts);
        tree.Label = label;
        return tree;
    }

    static ImmutableDictionary<TreeLabel, TokenTree> TreeDictionary(IEnumerable<TokenTree> trees)
    {
        var result = ImmutableDictionary<TreeLabel, TokenTree>.Empty;
        var count = 0;

        foreach (var tree in trees)
        {
            result = result.Add((TreeLabel)(--count), tree);
        }

        return result;
    }
}