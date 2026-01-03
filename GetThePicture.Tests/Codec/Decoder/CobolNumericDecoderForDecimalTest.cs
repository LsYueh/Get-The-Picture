using System.Globalization;

using GetThePicture.Cobol;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Decoder;

[TestClass]
public class CobolNumericDecoderForDecimalTest
{
    [TestMethod]
    [DataTestMethod]
    [DataRow( "12345",  "9(3)V9(2)", typeof(decimal),  "123.45")]
    [DataRow( "1234E", "S9(3)V9(2)", typeof(decimal),  "123.45")]
    [DataRow( "1234N", "S9(3)V9(2)", typeof(decimal), "-123.45")]
    public void Decode_Default_Decimal(string display, string picString, Type expectedType, string expectedValue)
    {
        var pic = Pic.Parse(picString);

        var value = CobolValueCodec.Build(display, pic).Decode();

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(decimal.Parse(expectedValue, CultureInfo.InvariantCulture), value);
    }
}