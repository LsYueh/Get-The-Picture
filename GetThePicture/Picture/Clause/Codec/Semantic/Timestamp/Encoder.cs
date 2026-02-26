using System.Globalization;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Codec.Semantic.Timestamp;

internal static class Encoder
{
    public static byte[] Encode(object value, PicMeta pic)
    {
        if (value is not DateTime dt)
            throw new FormatException($"Invalid value type for Timestamp (DateTime) encoding: {value.GetType().FullName}");

        if (pic.Semantic != PicSemantic.Timestamp14)
        {
            throw new NotSupportedException($"DateTime can only be encoded as Timestamp14, but was {pic.Semantic}");
        }

        string s = dt.ToString("yyyyMMddHHmmss", CultureInfo.InvariantCulture);

        return System.Text.Encoding.ASCII.GetBytes(s);
    }
}