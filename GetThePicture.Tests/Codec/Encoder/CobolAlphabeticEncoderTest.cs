using GetThePicture.Cobol;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Encoder;

[TestClass]
public class CobolAlphabeticEncoderTest
{
    [TestMethod]
    public void Encode_Alphabetic()
    {
        var pic = Pic.Parse("A(5)");
        string result = CobolValueCodec.ForPic(pic).Encode("AbC");

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphabetic_Extra()
    {
        var pic = Pic.Parse("A(5)");
        string result = CobolValueCodec.ForPic(pic).Encode("AbC  fGh");

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

        CobolValueCodec.ForPic(pic).Encode("AbC@ ");
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Encode_Numeric_ThrowsFormatException()
    {
        var pic = Pic.Parse("A(5)");

        CobolValueCodec.ForPic(pic).Encode("12345");
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Encode_CP950_ThrowsFormatException()
    {
        var pic = Pic.Parse("A(7)");

        CobolValueCodec.ForPic(pic).Encode("中文字 ");
    }
}