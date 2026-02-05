using System.Text;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Encoder.Category;

[TestClass]
public class AlphabeticEncoderTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void Encode_Alphabetic()
    {
        var pic = PicMeta.Parse("A(5)");
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode("AbC");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphabetic_Extra()
    {
        var pic = PicMeta.Parse("A(5)");
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode("AbC  fGh");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("AbC  ", result);
    }

    // -------------------------
    // Invalid format
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Encode_Alphanumeric_ThrowsFormatException()
    {
        var pic = PicMeta.Parse("A(5)");

        PicClauseCodec.ForMeta(pic).Encode("AbC@ ");
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Encode_Numeric_ThrowsFormatException()
    {
        var pic = PicMeta.Parse("A(5)");

        PicClauseCodec.ForMeta(pic).Encode("12345");
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Encode_CP950_ThrowsFormatException()
    {
        var pic = PicMeta.Parse("A(7)");

        PicClauseCodec.ForMeta(pic).Encode("中文字 ");
    }
}