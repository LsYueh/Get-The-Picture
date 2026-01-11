using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Encoder.Semantic;

[TestClass]
public class DateOnlyEncoderTest
{
    [TestMethod]
    [DataTestMethod]
    [DataRow("X(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("9(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("X(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    [DataRow("9(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    public void Encode_DateOnly(string picString, PicSemantic semantic, string expected, int year, int month, int day)
    {
        var pic = Pic.Parse(picString);
        pic.Semantic = semantic;

        var value = new DateOnly(year, month, day);
        string result = CodecBuilder.ForPic(pic).Encode(value);

        Assert.AreEqual(expected, result);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("X(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("9(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("X(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    [DataRow("9(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    public void Encode_DateOnly_WithSemantic(string picString, PicSemantic semantic, string expected, int year, int month, int day)
    {
        var pic = Pic.Parse(picString);

        var value = new DateOnly(year, month, day);
        string result = CodecBuilder.ForPic(pic).WithSemantic(semantic).Encode(value);

        Assert.AreEqual(expected, result);
    }
}
