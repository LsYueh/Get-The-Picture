using System.Text;

using GetThePicture.Picture;
using GetThePicture.Picture.Base.Clause.Items;
using GetThePicture.Picture.Base.Meta;
using GetThePicture.Picture.Utils;

namespace GetThePicture.Tests.Picture.Codec.Category.Alphanumeric;

[TestClass]
public class EncoderTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void Encode_Alphanumeric()
    {
        var pic = PicMeta.Parse("X(5)");
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode("AbC");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_Extra()
    {
        var pic = PicMeta.Parse("X(5)");
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode("AbC  fGh");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("AbC  ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_CP950()
    {
        var pic = PicMeta.Parse("X(7)");
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode("中文字");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("中文字 ", result);
    }

    [TestMethod]
    public void Encode_Alphanumeric_CP950_Lesser()
    {
        var pic = PicMeta.Parse("X(5)");
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode("中文字");

        string result = cp950.GetString(buffer);

        Assert.AreEqual("中文?", result);
    }

    [TestMethod]
    public void Encode_Wrong_Usage_ThrowsNotSupportedException()
    {
        var pic = PicMeta.Parse("X(7)");

        Assert.ThrowsExactly<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Usage(PicUsage.PackedDecimal).Encode("中文字 "));
    }
}