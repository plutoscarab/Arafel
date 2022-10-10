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
    Tursor? Token,
    ImmutableDictionary<TreeLabel, TokenTree> Parts)
{
    public TreeLabel Label { get; private set; } = TreeLabel.Ignore;

    public TokenTree(TreeKind kind, Tursor? tursor)
    : this(kind, tursor, ImmutableDictionary<TreeLabel, TokenTree>.Empty)
    { }

    public TokenTree(TreeKind kind, Tursor? tursor, IEnumerable<TokenTree> trees)
    : this(kind, tursor, TreeDictionary(trees))
    { }

    public TokenTree(TreeKind kind, IEnumerable<TokenTree> trees)
    : this(kind, null, TreeDictionary(trees))
    { }

    public TokenTree(TreeKind kind, params (TreeLabel Label, TokenTree Tree)[] trees)
    : this(kind, null, trees.ToDictionary(t => t.Label, t => t.Tree).ToImmutableDictionary())
    { }

    public TokenTree Tag(TreeLabel label)
    {
        var tree = new TokenTree(this.Kind, this.Token, this.Parts);
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