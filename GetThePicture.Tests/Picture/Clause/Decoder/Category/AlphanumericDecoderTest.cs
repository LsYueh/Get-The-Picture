using System.Text;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Decoder.Category;

[TestClass]
public class AlphanumericDecoderTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
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

        byte[] buffer = cp950.GetBytes("中文字 ");

        object result = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.AreEqual("中文字", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_CP950_Lesser_TrimsRightSpaces()
    {
        var pic = PicMeta.Parse("X(5)");
        
        byte[] buffer = cp950.GetBytes("中文字 ");

        object result = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.AreEqual("中文?", result);
    }

    [TestMethod]
    [ExpectedException(typeof(NotSupportedException))]
    public void Decode_Wrong_Usage_ThrowsNotSupportedException()
    {
        var pic = PicMeta.Parse("X(7)");

        byte[] buffer = cp950.GetBytes("中文字 ");

        PicClauseCodec.ForMeta(pic).Usage(PicUsage.Binary).Decode(buffer);
    }
}
