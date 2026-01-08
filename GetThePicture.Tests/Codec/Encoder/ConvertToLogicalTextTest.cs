using GetThePicture.Cobol;
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
    public void Decimal_UsesInvariantCulture()
    {
        string text = CobolPicEecoder.ConvertToLogicalText(12.34m, Pic.Parse("9(5)V9(2)"));
        Assert.AreEqual("12.34", text);
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

    // Unsupported type
    [TestMethod]
    public void UnsupportedType_Throws()
    {
        Assert.ThrowsException<NotSupportedException>(() =>
            CobolPicEecoder.ConvertToLogicalText(new object(), Pic.Parse("X(5)")));
    }
}
