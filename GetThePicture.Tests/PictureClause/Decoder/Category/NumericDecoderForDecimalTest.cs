using System.Globalization;
using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.PictureClause;

namespace GetThePicture.Tests.PictureClause.Decoder.Category;

[TestClass]
public class NumericDecoderForDecimalTest
{
    [TestMethod]
    [DataTestMethod]
    [DataRow( "01234",  "9(3)V9(2)", typeof(decimal),  "12.34")]
    [DataRow( "12345",  "9(3)V9(2)", typeof(decimal),  "123.45")]
    [DataRow( "1234E", "S9(3)V9(2)", typeof(decimal),  "123.45")]
    [DataRow( "1234N", "S9(3)V9(2)", typeof(decimal), "-123.45")]
    public void Decode_Default_Decimal(string text, string picString, Type expectedType, string expectedValue)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(decimal.Parse(expectedValue, CultureInfo.InvariantCulture), value);
    }
}