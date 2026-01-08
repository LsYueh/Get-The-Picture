using System.Globalization;

using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Encoder;

namespace GetThePicture.Tests.Codec.Encoder;

[TestClass]
public class ConvertToLogicalText
{

    [TestMethod]
    public void StringChar_PassThrough()
    {
        Assert.AreEqual("Hello", CobolPicEecoder.ConvertToLogicalText("Hello", Pic.Parse("X(5)")));
        Assert.AreEqual("A"    , CobolPicEecoder.ConvertToLogicalText('A'    , Pic.Parse("X(1)")));
    }

    [TestMethod]
    public void Integer_PreservesSign()
    {
        string text = CobolPicEecoder.ConvertToLogicalText(-123, Pic.Parse("9(5)"));
        Assert.AreEqual("-123", text);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow( "12.3", "9(5)V9(2)",  "1230")]
    [DataRow("-12.3", "9(5)V9(2)", "-1230")]
    [DataRow("-12.3", "9(1)V9(3)", "-12300")]
    public void Decimal(string value, string picString, string expected)
    {
        var _value = decimal.Parse(value, CultureInfo.InvariantCulture);
        string text = CobolPicEecoder.ConvertToLogicalText(_value, Pic.Parse(picString));
        Assert.AreEqual(expected, text);
    }

    [TestMethod]
    public void DateOnly_Gregorian8_ToLogicalText()
    {
        var pic = Pic.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;
        
        var date = new DateOnly(2026, 1, 5);
        string text = CobolPicEecoder.ConvertToLogicalText(date, pic);
        Assert.AreEqual("20260105", text);
    }

    [TestMethod]
    public void DateOnly_Minguo7_ToLogicalText()
    {
        var pic = Pic.Parse("9(8)");
        pic.Semantic = PicSemantic.MinguoDate;
        
        var date = new DateOnly(2026, 1, 5);
        string text = CobolPicEecoder.ConvertToLogicalText(date, pic);
        Assert.AreEqual("1150105", text);
    }

    [TestMethod]
    public void TimeOnly_Time6_PadsZero()
    {
        var pic = Pic.Parse("9(6)");
        pic.Semantic = PicSemantic.Time6;

        var t = new TimeOnly(1, 2, 3);
        Assert.AreEqual("010203", CobolPicEecoder.ConvertToLogicalText(t, pic));
    }

    [TestMethod]
    public void TimeOnly_Time9_MillisecondPadding()
    {
        var pic = Pic.Parse("9(9)");
        pic.Semantic = PicSemantic.Time9;

        var t = new TimeOnly(23, 59, 59, 7);
        Assert.AreEqual("235959007", CobolPicEecoder.ConvertToLogicalText(t, pic));
    }

    [TestMethod]
    public void DateTime_Timestamp14()
    {
        var pic = Pic.Parse("9(14)");
        pic.Semantic = PicSemantic.Timestamp14;

        var dt = new DateTime(2025, 1, 6, 13, 45, 59);
        Assert.AreEqual("20250106134559", CobolPicEecoder.ConvertToLogicalText(dt, pic));
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    public void DateTime_Reject_Time6()
    {
        var pic = Pic.Parse("9(6)");
        pic.Semantic = PicSemantic.Time6;

        Assert.ThrowsException<NotSupportedException>(() => CobolPicEecoder.ConvertToLogicalText(new DateTime(), pic));
    }

    [TestMethod]
    public void DateTime_Reject_Date_Gregorian8()
    {
        var pic = Pic.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        Assert.ThrowsException<NotSupportedException>(() => CobolPicEecoder.ConvertToLogicalText(new DateTime(), pic));
    }

    [TestMethod]
    public void Decimal_Has_Fraction_But_Expected_Integer()
    {
        var pic = Pic.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        Assert.ThrowsException<InvalidOperationException>(() => CobolPicEecoder.ConvertToLogicalText(12345.6m, pic));
    }

    // Unsupported type
    [TestMethod]
    public void UnsupportedType_Throws()
    {
        Assert.ThrowsException<NotSupportedException>(() =>
            CobolPicEecoder.ConvertToLogicalText(new object(), Pic.Parse("X(5)")));
    }
}
