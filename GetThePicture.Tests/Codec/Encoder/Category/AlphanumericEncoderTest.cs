using GetThePicture.Codec;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Tests.Codec.Encoder.Category;

[TestClass]
public class AlphanumericEncoderTest
{
    [TestMethod]
    public void Encode_Alphanumeric()
    {
        var pic = Pic.Parse("X(5)");
        string result = CodecBuilder.ForPic(pic).Encode("AbC");

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_Extra()
    {
        var pic = Pic.Parse("X(5)");
        string result = CodecBuilder.ForPic(pic).Encode("AbC  fGh");

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_CP950()
    {
        var pic = Pic.Parse("X(7)");
        string result = CodecBuilder.ForPic(pic).Encode("中文字");

        Assert.AreEqual("中文字 ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_CP950_Lesser()
    {
        var pic = Pic.Parse("X(5)");
        string result = CodecBuilder.ForPic(pic).Encode("中文字");

        Assert.AreEqual("中文?", result);
    }
}