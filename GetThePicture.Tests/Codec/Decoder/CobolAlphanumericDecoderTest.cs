using GetThePicture.Cobol;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Decoder;

[TestClass]
public class CobolAlphanumericDecoderTest
{
    [TestMethod]
    public void Decode_Alphanumeric_TrimsRightSpaces()
    {
        var pic = Pic.Parse("X(5)");
        object result = CobolValueCodec.ForPic(pic).Decode("ABC  ");

        Assert.AreEqual("ABC", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_Lesser_Extra_TrimsRightSpaces()
    {
        var pic = Pic.Parse("X(6)");
        object result = CobolValueCodec.ForPic(pic).NoStrict().Decode("ABC  ");

        Assert.AreEqual("ABC", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_CP950_TrimsRightSpaces()
    {
        var pic = Pic.Parse("X(7)");
        object result = CobolValueCodec.ForPic(pic).Decode("中文字 ");

        Assert.AreEqual("中文字", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_CP950_Lesser_TrimsRightSpaces()
    {
        var pic = Pic.Parse("X(5)");
        object result = CobolValueCodec.ForPic(pic).NoStrict().Decode("中文字 ");

        Assert.AreEqual("中文?", result);
    }
}
