using System.Text;

using GetThePicture.PictureClause;
using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Utils;

namespace GetThePicture.Tests.PictureClause.Encoder.Category;

[TestClass]
public class NumericEncoderForIntegerTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    [DataTestMethod]
    [DataRow((byte)                  99, "9(02)",                 "99")]
    [DataRow((ushort)              9999, "9(04)",               "9999")]
    [DataRow((uint)           999999999, "9(09)",          "999999999")]
    [DataRow((ulong) 999999999999999999, "9(18)", "999999999999999999")]
    public void Encode_Integer_Default(object value, string picString, string expected)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(value);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }
}
