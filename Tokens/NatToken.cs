using System.Numerics;

internal sealed record NatToken(Cursor Start, int Length, BigInteger Value) 
: Token(Start, Length)
{
    public override string ToString() =>
        Value.ToString();

    public static BigInteger UnicodeParse(string s)
    {
        BigInteger b = 0;
        var index = 0;

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