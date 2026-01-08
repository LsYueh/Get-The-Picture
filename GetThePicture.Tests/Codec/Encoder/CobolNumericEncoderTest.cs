using System.Globalization;

using GetThePicture.Cobol;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Encoder;

[TestClass]
public class CobolNumericEncoderTest
{
    [TestMethod]
    [DataTestMethod]
    [DataRow( 123.45, "9(3)V9(2)", "12345")]
    [DataRow( 123.45, "9(3)V9(5)", "12345000")]
    [DataRow( 123.45, "9(2)V9(2)",  "2345")]
    [DataRow(  12.3 , "9(1)V9(3)",  "2300")]
    // TODO: S9
    public void Encode_Double_Default(object value, string picString, string expected)
    {
        var pic = Pic.Parse(picString);
        string result = CobolValueCodec.ForPic(pic).Encode(value);

        Assert.AreEqual(expected, result);
    }
}
