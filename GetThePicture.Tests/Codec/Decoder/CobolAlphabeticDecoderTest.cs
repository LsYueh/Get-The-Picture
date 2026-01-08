using GetThePicture.Cobol;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Decoder;

[TestClass]
public class CobolAlphabeticDecoderTest
{
    [TestMethod]
    public void Decode_Alphabetic_TrimsRightSpaces()
    {
        var pic = Pic.Parse("A(5)");
        object result = CobolValueCodec.ForPic(pic).Decode("AbC  ");

        Assert.AreEqual("AbC", result);
    }

    [TestMethod]
    public void Decode_Alphabetic_Lesser_Extra_TrimsRightSpaces()
    {
        var pic = Pic.Parse("A(5)");
        object result = CobolValueCodec.ForPic(pic).NoStrict().Decode("AbC  fGh");

        Assert.AreEqual("AbC", result);
    }

    // -------------------------
    // Invalid format
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_Alphanumeric_ThrowsFormatException()
    {
        var pic = Pic.Parse("A(5)");

        CobolValueCodec.ForPic(pic).Decode("AbC@ ");
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_Numeric_ThrowsFormatException()
    {
        var pic = Pic.Parse("A(5)");

        CobolValueCodec.ForPic(pic).Decode("12345");
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_CP950_ThrowsFormatException()
    {
        var pic = Pic.Parse("A(7)");

        CobolValueCodec.ForPic(pic).Decode("中文字 ");
    }
}
