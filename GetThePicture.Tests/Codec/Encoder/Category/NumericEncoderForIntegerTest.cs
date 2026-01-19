using System.Globalization;

using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Encoder.Category;

[TestClass]
public class NumericEncoderForIntegerTest
{
    [TestMethod]
    [DataTestMethod]
    [DataRow((byte)                  99, "9(02)",                 "99")]
    [DataRow((ushort)              9999, "9(04)",               "9999")]
    [DataRow((uint)           999999999, "9(09)",          "999999999")]
    [DataRow((ulong) 999999999999999999, "9(18)", "999999999999999999")]
    public void Encode_Integer_Default(object value, string picString, string expected)
    {
        var pic = Pic.Parse(picString);
        string result = CodecBuilder.ForPic(pic).Encode(value);

        Assert.AreEqual(expected, result);
    }
}
