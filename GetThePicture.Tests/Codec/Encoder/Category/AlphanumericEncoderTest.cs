using System.Text;

using GetThePicture.Codec;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Tests.Codec.Encoder.Category;

[TestClass]
public class AlphanumericEncoderTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void Encode_Alphanumeric()
    {
        var pic = Pic.Parse("X(5)");
        byte[] buffer = CodecBuilder.ForPic(pic).Encode("AbC");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_Extra()
    {
        var pic = Pic.Parse("X(5)");
        byte[] buffer = CodecBuilder.ForPic(pic).Encode("AbC  fGh");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_CP950()
    {
        var pic = Pic.Parse("X(7)");
        byte[] buffer = CodecBuilder.ForPic(pic).Encode("中文字");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("中文字 ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_CP950_Lesser()
    {
        var pic = Pic.Parse("X(5)");
        byte[] buffer = CodecBuilder.ForPic(pic).Encode("中文字");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("中文?", result);
    }
}