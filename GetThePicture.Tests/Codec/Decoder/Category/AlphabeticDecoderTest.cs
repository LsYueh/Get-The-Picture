using System.Text;

using GetThePicture.Codec;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Tests.Codec.Decoder.Category;

[TestClass]
public class AlphabeticDecoderTest
{
    [TestMethod]
    public void Decode_Alphabetic_TrimsRightSpaces()
    {
        var pic = Pic.Parse("A(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("AbC  ");

        object result = CodecBuilder.ForPic(pic).Decode(buffer);

        Assert.AreEqual("AbC", result);
    }

    [TestMethod]
    public void Decode_Alphabetic_Lesser_Extra_TrimsRightSpaces()
    {
        var pic = Pic.Parse("A(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("AbC  fGh");

        object result = CodecBuilder.ForPic(pic).Decode(buffer);

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
        byte[] buffer = Encoding.ASCII.GetBytes("AbC@ ");

        CodecBuilder.ForPic(pic).Decode(buffer);
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_Numeric_ThrowsFormatException()
    {
        var pic = Pic.Parse("A(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("12345");

        CodecBuilder.ForPic(pic).Decode(buffer);
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_CP950_ThrowsFormatException()
    {
        var pic = Pic.Parse("A(7)");

        Encoding cp950 = EncodingFactory.CP950;
        byte[] buffer = cp950.GetBytes("中文字 ");

        CodecBuilder.ForPic(pic).Decode(buffer);
    }
}
