
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Decoder.Semantic;

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
        string display,
        int year,
        int month,
        int day,
        int hour,
        int minute,
        int second)
    {
        var pic = Pic.Parse(picString);
        pic.Semantic = semantic;

        object result = CodecBuilder.ForPic(pic).Decode(display);

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
        var pic = Pic.Parse("S9(14)");
        pic.Semantic = PicSemantic.Timestamp14;

        var display = "20240115123045";

        Assert.ThrowsException<NotSupportedException>(() => CodecBuilder.ForPic(pic).Decode(display));
    }

    [TestMethod]
    public void Decode_InvalidTimestamp14_ThrowsFormatException()
    {
        var pic = Pic.Parse("9(14)");
        pic.Semantic = PicSemantic.Timestamp14;

        var display = "20241301120000"; // invalid month

        Assert.ThrowsException<FormatException>(() => CodecBuilder.ForPic(pic).Decode(display));
    }
}
