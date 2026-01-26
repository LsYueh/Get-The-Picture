using System.Text;

using GetThePicture.PictureClause;
using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Utils;

namespace GetThePicture.Tests.PictureClause.Decoder.Category;

[TestClass]
public class AlphabeticDecoderTest
{
    [TestMethod]
    public void Decode_Alphabetic_TrimsRightSpaces()
    {
        var pic = PicMeta.Parse("A(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("AbC  ");

        object result = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.AreEqual("AbC", result);
    }

    [TestMethod]
    public void Decode_Alphabetic_Lesser_Extra_TrimsRightSpaces()
    {
        var pic = PicMeta.Parse("A(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("AbC  fGh");

        object result = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.AreEqual("AbC", result);
    }

    // -------------------------
    // Invalid format
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_Alphanumeric_ThrowsFormatException()
    {
        var pic = PicMeta.Parse("A(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("AbC@ ");

        PicClauseCodec.ForMeta(pic).Decode(buffer);
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_Numeric_ThrowsFormatException()
    {
        var pic = PicMeta.Parse("A(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("12345");

        PicClauseCodec.ForMeta(pic).Decode(buffer);
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_CP950_ThrowsFormatException()
    {
        var pic = PicMeta.Parse("A(7)");

        Encoding cp950 = EncodingFactory.CP950;
        byte[] buffer = cp950.GetBytes("中文字 ");

        PicClauseCodec.ForMeta(pic).Decode(buffer);
    }
}
