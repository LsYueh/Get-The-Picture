using System.Globalization;
using System.Text;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Codec.Category.Numeric;

[TestClass]
public class EncoderForIntegerTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [DataTestMethod]
    [DataRow((byte)                  99, "9(02)",                 "99")]
    [DataRow((ushort)              9999, "9(04)",               "9999")]
    [DataRow((uint)           999999999, "9(09)",          "999999999")]
    [DataRow((ulong)                  1, "9(18)", "000000000000000001")]
    [DataRow((ulong) 999999999999999999, "9(18)", "999999999999999999")]
    public void Encode_Integer_Default(object value, string picString, string expected)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(value);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }

    [DataTestMethod]
    [DataRow(                           "9", "9(28)", "0000000000000000000000000009")]
    [DataRow( "999999999999999999999999999", "9(27)",  "999999999999999999999999999")]
    [DataRow( "999999999999999999999999999", "9(28)", "0999999999999999999999999999")]
    [DataRow("9999999999999999999999999999", "9(28)", "9999999999999999999999999999")]
    public void Encode_Integer_28(string value, string picString, string expected)
    {
        decimal v = decimal.Parse(value, CultureInfo.InvariantCulture);

        var pic = PicMeta.Parse(picString);
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(v);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }
}
