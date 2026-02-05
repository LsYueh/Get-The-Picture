using System.Text;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Encoder.Semantic;

[TestClass]
public class TimestampEncoderTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [DataTestMethod]
    [DataRow("X(14)", PicSemantic.Timestamp14, "20240115123045", 2024,  1, 15, 12, 30, 45)]
    [DataRow("9(14)", PicSemantic.Timestamp14, "20240115123045", 2024,  1, 15, 12, 30, 45)]
    [DataRow("X(14)", PicSemantic.Timestamp14, "19991231235959", 1999, 12, 31, 23, 59, 59)]
    [DataRow("9(14)", PicSemantic.Timestamp14, "19991231235959", 1999, 12, 31, 23, 59, 59)]
    public void Decode_Timestamp14_FromDataRow(
        string picString,
        PicSemantic semantic,
        string expected,
        int year,
        int month,
        int day,
        int hour,
        int minute,
        int second)
    {
        var pic = PicMeta.Parse(picString);
        pic.Semantic = semantic;
        
        var value = new DateTime(
            year, month, day,
            hour, minute, second,
            DateTimeKind.Unspecified);
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(value);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }
}
