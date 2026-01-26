using System.Text;

using GetThePicture.Cobol.Utils;
using GetThePicture.PictureClause;

namespace GetThePicture.Tests.PictureClause.Encoder.Category;

[TestClass]
public class AlphabeticEncoderTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void Encode_Alphabetic()
    {
        var pic = Pic.Parse("A(5)");
        byte[] buffer = CodecBuilder.ForPic(pic).Encode("AbC");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphabetic_Extra()
    {
        var pic = Pic.Parse("A(5)");
        byte[] buffer = CodecBuilder.ForPic(pic).Encode("AbC  fGh");

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
        var pic = Pic.Parse("A(5)");

        CodecBuilder.ForPic(pic).Encode("AbC@ ");
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Encode_Numeric_ThrowsFormatException()
    {
        var pic = Pic.Parse("A(5)");

        CodecBuilder.ForPic(pic).Encode("12345");
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Encode_CP950_ThrowsFormatException()
    {
        var pic = Pic.Parse("A(7)");

        CodecBuilder.ForPic(pic).Encode("中文字 ");
    }
}