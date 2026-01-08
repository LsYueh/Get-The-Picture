using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Encoder;

[TestClass]
public class CobolAlphanumericEncoderTest
{
    [TestMethod]
    public void Encode_Alphanumeric()
    {
        var pic = Pic.Parse("X(5)");
        string result = CobolValueCodec.ForPic(pic).Encode("AbC");

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_Extra()
    {
        var pic = Pic.Parse("X(5)");
        string result = CobolValueCodec.ForPic(pic).Encode("AbC  fGh");

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_CP950()
    {
        var pic = Pic.Parse("X(7)");
        string result = CobolValueCodec.ForPic(pic).Encode("中文字");

        Assert.AreEqual("中文字 ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_CP950_Lesser()
    {
        var pic = Pic.Parse("X(5)");
        string result = CobolValueCodec.ForPic(pic).Encode("中文字");

        Assert.AreEqual("中文?", result);
    }
}