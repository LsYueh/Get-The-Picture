using System.Text;

using GetThePicture.PictureClause;
using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.ClauseItems;

namespace GetThePicture.Tests.PictureClause.Decoder.Semantic;

[TestClass]
public class TimestampDecoderTest
{
    [DataTestMethod]
    [DataRow("X(14)", PicSemantic.Timestamp14, "20240115123045", 2024,  1, 15, 12, 30, 45)]
    [DataRow("9(14)", PicSemantic.Timestamp14, "20240115123045", 2024,  1, 15, 12, 30, 45)]
    [DataRow("X(14)", PicSemantic.Timestamp14, "19991231235959", 1999, 12, 31, 23, 59, 59)]
    [DataRow("9(14)", PicSemantic.Timestamp14, "19991231235959", 1999, 12, 31, 23, 59, 59)]
    public void Decode_Timestamp14_FromDataRow(
        string picString,
        PicSemantic semantic,
        string text,
        int year,
        int month,
        int day,
        int hour,
        int minute,
        int second)
    {
        var pic = PicMeta.Parse(picString);
        pic.Semantic = semantic;

        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object result = PicClauseCodec.ForMeta(pic).Decode(buffer);

        var expected = new DateTime(
            year, month, day,
            hour, minute, second,
            DateTimeKind.Unspecified);

        Assert.AreEqual(expected, result);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    public void Decode_SignedNumeric_ThrowsNotSupportedException()
    {
        var pic = PicMeta.Parse("S9(14)");
        pic.Semantic = PicSemantic.Timestamp14;

        byte[] buffer = Encoding.ASCII.GetBytes("20240115123045");

        Assert.ThrowsException<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Decode(buffer));
    }

    [TestMethod]
    public void Decode_InvalidTimestamp14_ThrowsFormatException()
    {
        var pic = PicMeta.Parse("9(14)");
        pic.Semantic = PicSemantic.Timestamp14;

        byte[] buffer = Encoding.ASCII.GetBytes("20241301120000");// invalid month

        Assert.ThrowsException<FormatException>(() => PicClauseCodec.ForMeta(pic).Decode(buffer));
    }
}
