
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Decoder.Semantic;

[TestClass]
public class TimeOnlyDecoderTest
{
    [DataTestMethod]
    [DataRow("X(6)", PicSemantic.Time6, "235959",    23, 59, 59, 0)]   // Time6
    [DataRow("9(6)", PicSemantic.Time6, "235959",    23, 59, 59, 0)]
    [DataRow("X(9)", PicSemantic.Time9, "123045678", 12, 30, 45, 678)] // Time9
    [DataRow("9(9)", PicSemantic.Time9, "123045678", 12, 30, 45, 678)] 
    public void Decode_TimeOnly_FromDataRow(
        string picString,
        PicSemantic semantic,
        string display,
        int hour, int minute, int second, int millisecond)
    {
        var pic = Pic.Parse(picString);
        pic.Semantic = semantic;

        object result = CodecBuilder.ForPic(pic).Decode(display);

        var expected = new TimeOnly(hour, minute, second, millisecond);
        Assert.AreEqual(expected, result);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    public void Decode_SignedNumeric_ThrowsNotSupportedException()
    {
        var pic = Pic.Parse("S9(6)");
        pic.Semantic = PicSemantic.Time6;

        var display = "123456";

        Assert.ThrowsException<NotSupportedException>(() => CodecBuilder.ForPic(pic).Decode(display));
    }

    [TestMethod]
    public void Decode_InvalidTime6_ThrowsFormatException()
    {
        var pic = Pic.Parse("S9(6)");
        pic.Semantic = PicSemantic.Time6;

        var display = "246060"; // invalid time

        Assert.ThrowsException<NotSupportedException>(() => CodecBuilder.ForPic(pic).Decode(display));
    }
}
