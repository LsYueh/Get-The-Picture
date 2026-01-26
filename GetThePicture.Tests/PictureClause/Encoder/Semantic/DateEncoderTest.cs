using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Cobol.Utils;
using GetThePicture.PictureClause;

namespace GetThePicture.Tests.PictureClause.Encoder.Semantic;

[TestClass]
public class DateEncoderTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    [DataTestMethod]
    [DataRow("X(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("9(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("X(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    [DataRow("9(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    public void Encode_DateOnly(string picString, PicSemantic semantic, string expected, int year, int month, int day)
    {
        var pic = PicMeta.Parse(picString);
        pic.Semantic = semantic;

        var value = new DateOnly(year, month, day);
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(value);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("X(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("9(8)", PicSemantic.GregorianDate, "20240115", 2024, 1, 15)]
    [DataRow("X(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    [DataRow("9(7)", PicSemantic.MinguoDate   ,  "1130115", 2024, 1, 15)]
    public void Encode_DateOnly_AsSemantic(string picString, PicSemantic semantic, string expected, int year, int month, int day)
    {
        var pic = PicMeta.Parse(picString);

        var value = new DateOnly(year, month, day);
        byte[] buffer = PicClauseCodec.ForMeta(pic).AsSemantic(semantic).Encode(value);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }
}
