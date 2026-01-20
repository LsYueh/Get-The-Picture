using System.Globalization;

using GetThePicture.Codec;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Tests.Codec.Decoder.Category;

[TestClass]
public class NumericDecoderForDecimalTest
{
    [TestMethod]
    [DataTestMethod]
    [DataRow( "01234",  "9(3)V9(2)", typeof(decimal),  "12.34")]
    [DataRow( "12345",  "9(3)V9(2)", typeof(decimal),  "123.45")]
    [DataRow( "1234E", "S9(3)V9(2)", typeof(decimal),  "123.45")]
    [DataRow( "1234N", "S9(3)V9(2)", typeof(decimal), "-123.45")]
    public void Decode_Default_Decimal(string display, string picString, Type expectedType, string expectedValue)
    {
        var pic = Pic.Parse(picString);
        object value = CodecBuilder.ForPic(pic).Decode(display);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(decimal.Parse(expectedValue, CultureInfo.InvariantCulture), value);
    }
}