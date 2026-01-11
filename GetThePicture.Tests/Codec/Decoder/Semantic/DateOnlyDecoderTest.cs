using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Decoder.Semantic;

[TestClass]
public class DateOnlyDecoderTest
{
    [TestMethod]
    [DataTestMethod]
    [DataRow("X(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("9(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("X(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    [DataRow("9(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    public void Decode_DateOnly(string picString, PicSemantic semantic, string display, int year, int month, int day)
    {
        var pic = Pic.Parse(picString);
        pic.Semantic = semantic;

        object result = CodecBuilder.ForPic(pic).Decode(display);

        var expected = new DateOnly(year, month, day);
        Assert.AreEqual(expected, result);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("X(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("9(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("X(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    [DataRow("9(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    public void Decode_DateOnly_WithSemantic(string picString, PicSemantic semantic, string display, int year, int month, int day)
    {
        var pic = Pic.Parse(picString);

        object result = CodecBuilder.ForPic(pic).WithSemantic(semantic).Decode(display);

        var expected = new DateOnly(year, month, day);
        Assert.AreEqual(expected, result);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    public void Decode_InvalidBaseType()
    {
        var pic = Pic.Parse("S9(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        Assert.ThrowsException<NotSupportedException>(() => CodecBuilder.ForPic(pic).Decode("20241301"));
    }

    [TestMethod]
    public void Decode_InvalidGregorianDate_ThrowsFormatException()
    {
        var pic = Pic.Parse("X(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        Assert.ThrowsException<FormatException>(() => CodecBuilder.ForPic(pic).Decode("20241301"));
    }
}
