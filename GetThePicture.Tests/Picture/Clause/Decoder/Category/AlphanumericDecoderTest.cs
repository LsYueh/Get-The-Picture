using System.Text;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Decoder.Category;

[TestClass]
public class AlphanumericDecoderTest
{
    [TestMethod]
    public void Decode_Alphanumeric_TrimsRightSpaces()
    {
        var pic = PicMeta.Parse("X(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("ABC  ");

        object result = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.AreEqual("ABC", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_Lesser_Extra_TrimsRightSpaces()
    {
        var pic = PicMeta.Parse("X(6)");
        byte[] buffer = Encoding.ASCII.GetBytes("ABC  ");

        object result = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.AreEqual("ABC", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_CP950_TrimsRightSpaces()
    {
        var pic = PicMeta.Parse("X(7)");

        Encoding cp950 = EncodingFactory.CP950;
        byte[] buffer = cp950.GetBytes("中文字 ");

        object result = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.AreEqual("中文字", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_CP950_Lesser_TrimsRightSpaces()
    {
        var pic = PicMeta.Parse("X(5)");
        
        Encoding cp950 = EncodingFactory.CP950;
        byte[] buffer = cp950.GetBytes("中文字 ");

        object result = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.AreEqual("中文?", result);
    }
}
