using System.Text;
using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Tests.Picture.Clause.Codec.Semantic.Boolean;

[TestClass]
public class DecoderTest
{
    [TestMethod]
    [DataRow("X(1)", "Y", true)]
    [DataRow("X(1)", "N", false)]
    [DataRow("A(1)", "Y", true)]
    [DataRow("A(1)", "N", false)]
    [DataRow("9(1)", "1", true)]
    [DataRow("9(1)", "0", false)]
    [DataRow("X(1)", "y", true)]  // 測大小寫
    [DataRow("X(1)", "n", false)]
    public void Decode_Alpha_Test(string picString, string text, bool expected)
    {
        var pic = PicMeta.Parse(picString);
        pic.Semantic = PicSemantic.Boolean;

        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.AreEqual(expected, value);
    }

    // Exceptions

    [TestMethod]
    public void Decode_ShouldThrow_WhenUsageNotDisplay()
    {
        var pic = PicMeta.Parse("9(1)");
        pic.Semantic = PicSemantic.Boolean;
        pic.Usage = PicUsage.Binary; // 非 DISPLAY

        byte[] buffer = Encoding.ASCII.GetBytes("1");

        Assert.ThrowsExactly<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Decode(buffer));
    }

    [TestMethod]
    public void Decode_ShouldThrow_WhenStorageOccupiedNot1()
    {
        var pic = PicMeta.Parse("9(2)"); // 兩位
        pic.Semantic = PicSemantic.Boolean;

        byte[] buffer = Encoding.ASCII.GetBytes("12");

        Assert.ThrowsExactly<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Decode(buffer));
    }

    [TestMethod]
    public void Decode_ShouldThrow_WhenInvalidNumericValue()
    {
        var pic = PicMeta.Parse("9(1)");
        pic.Semantic = PicSemantic.Boolean;

        byte[] buffer = Encoding.ASCII.GetBytes("2"); // 不合法值

        Assert.ThrowsExactly<FormatException>(() => PicClauseCodec.ForMeta(pic).Decode(buffer));
    }

    [TestMethod]
    public void Decode_ShouldThrow_WhenInvalidAlphanumericValue()
    {
        var pic = PicMeta.Parse("X(1)");
        pic.Semantic = PicSemantic.Boolean;

        byte[] buffer = Encoding.ASCII.GetBytes("Z"); // 不合法值

        Assert.ThrowsExactly<FormatException>(() => PicClauseCodec.ForMeta(pic).Decode(buffer));
    }

    [TestMethod]
    public void Decode_ShouldThrow_WhenSigned()
    {
        var pic = PicMeta.Parse("S9(1)"); // 帶符號
        pic.Semantic = PicSemantic.Boolean;

        byte[] buffer = Encoding.ASCII.GetBytes("1");

        Assert.ThrowsExactly<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Decode(buffer));
    }
}
