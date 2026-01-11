
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Encoder.Semantic;

[TestClass]
public class TimeEncoderTest
{
    [DataTestMethod]
    [DataRow("X(6)", PicSemantic.Time6, "235959",    23, 59, 59, 0)]   // Time6
    [DataRow("9(6)", PicSemantic.Time6, "235959",    23, 59, 59, 0)]
    [DataRow("X(9)", PicSemantic.Time9, "123045678", 12, 30, 45, 678)] // Time9
    [DataRow("9(9)", PicSemantic.Time9, "123045678", 12, 30, 45, 678)] 
    public void Encode_TimeOnly_FromDataRow(
        string picString,
        PicSemantic semantic,
        string expected,
        int hour, int minute, int second, int millisecond)
    {
        var pic = Pic.Parse(picString);
        pic.Semantic = semantic;

        var value = new TimeOnly(hour, minute, second, millisecond);
        object result = CodecBuilder.ForPic(pic).Encode(value);

        Assert.AreEqual(expected, result);
    }
}
