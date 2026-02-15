using System.Text;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Tests.Picture.Clause.Codec.Semantic.Date;

[TestClass]
public class DecoderTest
{
    [DataTestMethod]
    [DataRow("X(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("9(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("X(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    [DataRow("9(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    public void Decode_DateOnly(string picString, PicSemantic semantic, string text, int year, int month, int day)
    {
        var pic = PicMeta.Parse(picString);
        pic.Semantic = semantic;

        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        var expected = new DateOnly(year, month, day);
        Assert.AreEqual(expected, value);
    }

    [DataTestMethod]
    [DataRow("X(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("9(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("X(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    [DataRow("9(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    public void Decode_DateOnly_AsSemantic(string picString, PicSemantic semantic, string text, int year, int month, int day)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object result = PicClauseCodec.ForMeta(pic).AsSemantic(semantic).Decode(buffer);

        var expected = new DateOnly(year, month, day);
        Assert.AreEqual(expected, result);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    public void Decode_InvalidBaseType()
    {
        var pic = PicMeta.Parse("S9(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        byte[] buffer = Encoding.ASCII.GetBytes("20241301");

        Assert.ThrowsException<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Decode(buffer));
    }

    [TestMethod]
    public void Decode_Invalid_GregorianDate_ThrowsFormatException()
    {
        var pic = PicMeta.Parse("X(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        byte[] buffer = Encoding.ASCII.GetBytes("20241301");

        Assert.ThrowsException<FormatException>(() => PicClauseCodec.ForMeta(pic).Decode(buffer));
    }

    [DataTestMethod]
    [DataRow("X(7)", "11301CC")]
    [DataRow("9(7)", "11301CC")]
    [DataRow("X(7)", "113BBCC")]
    [DataRow("9(7)", "113BBCC")]
    [DataRow("X(7)", "AAABBCC")]
    [DataRow("9(7)", "AAABBCC")]
    [ExpectedException(typeof(FormatException))]
    public void Decode_Invalid_MinguoDate_ThrowsFormatException(string picString, string text)
    {
        var pic = PicMeta.Parse(picString);
        pic.Semantic = PicSemantic.MinguoDate;

        byte[] buffer = Encoding.ASCII.GetBytes(text);

        PicClauseCodec.ForMeta(pic).Decode(buffer);
    }
}
