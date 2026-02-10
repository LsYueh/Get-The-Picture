using System.Globalization;
using System.Text;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Tests.Picture.Clause.Decoder.Category;

[TestClass]
public class NumericDecoderForDecimalTest
{
    [DataTestMethod]
    [DataRow( "01234",  "9(3)V9(2)", typeof(decimal),  "12.34")]
    [DataRow( "12345",  "9(3)V9(2)", typeof(decimal),  "123.45")]
    [DataRow( "1234E", "S9(3)V9(2)", typeof(decimal),  "123.45")]
    [DataRow( "1234N", "S9(3)V9(2)", typeof(decimal), "-123.45")]
    [DataRow("9999999999999999999999999995",  "9(27)V9", typeof(decimal),  "999999999999999999999999999.5")]
    [DataRow("999999999999999999999999999E", "S9(27)V9", typeof(decimal),  "999999999999999999999999999.5")]
    [DataRow("999999999999999999999999999N", "S9(27)V9", typeof(decimal), "-999999999999999999999999999.5")]
    public void Decode_Default_Decimal(string text, string picString, Type expectedType, string expectedValue)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(decimal.Parse(expectedValue, CultureInfo.InvariantCulture), value);
    }
}