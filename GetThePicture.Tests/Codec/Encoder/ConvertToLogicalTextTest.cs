using GetThePicture.Cobol;
using GetThePicture.Codec.Encoder;

namespace GetThePicture.Tests.Codec.Encoder;

[TestClass]
public class ConvertToLogicalText
{
    // String / Char
    [TestMethod]
    public void StringChar_PassThrough()
    {
        Assert.AreEqual("Hello", CobolPicEecoder.ConvertToLogicalText("Hello", Pic.Parse("X(5)")));
        Assert.AreEqual("A"    , CobolPicEecoder.ConvertToLogicalText('A'    , Pic.Parse("X(1)")));
    }

    // Integer
    [TestMethod]
    public void Integer_PreservesSign()
    {
        string text = CobolPicEecoder.ConvertToLogicalText(-123, Pic.Parse("9(5)"));
        Assert.AreEqual("-123", text);
    }

    // Decimal
    [TestMethod]
    public void Decimal_UsesInvariantCulture()
    {
        string text = CobolPicEecoder.ConvertToLogicalText(12.34m, Pic.Parse("9(5)V9(2)"));
        Assert.AreEqual("12.34", text);
    }

    // DateTime
    [TestMethod]
    public void DateTime_ToLogicalText()
    {
        var dt = new DateTime(2026, 1, 5);
        string text = CobolPicEecoder.ConvertToLogicalText(dt, Pic.Parse("9(8)"));
        Assert.AreEqual("20260105", text);
    }

    // Unsupported type
    [TestMethod]
    public void UnsupportedType_Throws()
    {
        Assert.ThrowsException<NotSupportedException>(() =>
            CobolPicEecoder.ConvertToLogicalText(new object(), Pic.Parse("X(5)")));
    }
}
