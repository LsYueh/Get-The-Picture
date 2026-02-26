using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Codec.Semantic.Date;

internal static class Encoder
{    
    public static byte[] Encode(object value, PicMeta pic)
    {
        if (value is not DateOnly date)
            throw new FormatException($"Invalid value type for DateOnly encoding: {value.GetType().FullName}");

        Constraint rule = Rules.GetConstraint(pic.Semantic);
        rule.ValidateOrThrow(pic, pic.Semantic.ToString());

        string s = pic.Semantic switch
        {
            PicSemantic.GregorianDate => date.ToString("yyyyMMdd"),
            PicSemantic.MinguoDate    => ToMinguoDateString(date),
            _ => throw new NotSupportedException($"Unsupported DateOnly format: {pic.Semantic}")
        };

        return System.Text.Encoding.ASCII.GetBytes(s);
    }

    private static string ToMinguoDateString(DateOnly date)
    {
        int rocYear = date.Year - 1911;
        
        if (rocYear <= 0)
        {
            throw new ArgumentOutOfRangeException(nameof(date), "Date is before ROC calendar starts (1912-01-01).");
        }

        return $"{rocYear:000}{date:MMdd}";
    }
}