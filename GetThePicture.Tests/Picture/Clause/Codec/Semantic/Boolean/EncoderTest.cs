using System.Text;
using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Codec.Semantic.Boolean;

[TestClass]
public class EncoderTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    [DataRow("X(1)", "Y", true)]
    [DataRow("X(1)", "N", false)]
    [DataRow("A(1)", "Y", true)]
    [DataRow("A(1)", "N", false)]
    [DataRow("9(1)", "1", true)]
    [DataRow("9(1)", "0", false)]
    public void Decode_Alpha_Test(string picString, string expected, bool value)
    {
        var pic = PicMeta.Parse(picString);
        pic.Semantic = PicSemantic.Boolean;

        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(value);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }

    // Exceptions

    [TestMethod]
    public void Encode_ShouldThrow_WhenValueIsNotBool()
    {
        var pic = PicMeta.Parse("X(1)");
        pic.Semantic = PicSemantic.Boolean;

        object value = "NotABool"; // 非 bool

        Assert.ThrowsExactly<ArgumentException>(() => PicClauseCodec.ForMeta(pic).Encode(value));
    }

    [TestMethod]
    public void Encode_ShouldThrow_WhenUsageNotDisplay()
    {
        var pic = PicMeta.Parse("X(1)");
        pic.Semantic = PicSemantic.Boolean;
        pic.Usage = PicUsage.PackedDecimal; // 非 DISPLAY

        Assert.ThrowsExactly<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Encode(true));
    }

    [TestMethod]
    public void Encode_ShouldThrow_WhenStorageOccupiedNot1()
    {
        var pic = PicMeta.Parse("X(2)"); // 兩位
        pic.Semantic = PicSemantic.Boolean;

        Assert.ThrowsExactly<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Encode(true));
    }

    [TestMethod]
    public void Encode_ShouldThrow_WhenSigned()
    {
        var pic = PicMeta.Parse("S9(1)"); // 帶符號
        pic.Semantic = PicSemantic.Boolean;

        Assert.ThrowsExactly<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Encode(true));
    }

    [TestMethod]
    public void Encode_ShouldThrow_WhenUnsupportedPicType()
    {
        var pic = PicMeta.Parse("9(2)"); // Numeric 但長度 >1
        pic.Semantic = PicSemantic.Boolean;

        Assert.ThrowsExactly<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Encode(true));
    }
}