using System.Numerics;

internal sealed record NatToken(Cursor Start, int Length) 
: Token(Start, Length)
{
    public override string ToString() =>
        base.ToString();

    public override object Value =>
        BigInteger.TryParse(Text, out var value) ? value : UnicodeParse();

    private BigInteger UnicodeParse()
    {
        BigInteger b = 0;
        var index = 0;
        var s = Text;

        while (index < s.Length)
        {
            if (s[index] == '_')
            {
                ++index;
                continue;
            }
            
            b = b * 10 + (int)char.GetNumericValue(s, index);
            index += char.IsSurrogatePair(s, index) ? 2 : 1;
        }

        return b;
    }
}