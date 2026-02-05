using System.Globalization;

using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.ClauseItems;
using GetThePicture.PictureClause.Encoder;
using GetThePicture.PictureClause.Encoder.Meta;

namespace GetThePicture.Tests.PictureClause.Encoder;

[TestClass]
public class ToMetaTest
{

    [TestMethod]
    [DataTestMethod]
    [DataRow("X(5)", "Hello",  "Hello")]
    [DataRow("X(1)", "A",  "A")]
    public void StringChar_PassThrough(string picString, string value, string expected)
    {
        var pic = PicMeta.Parse(picString);

        CobMeta v = PicEncoder.ToCobMeta(value, pic);

        Assert.AreEqual(expected, v.Text?.Value);
    }

    [TestMethod]
    public void Integer_PreservesSign()
    {
        var pic = PicMeta.Parse("9(5)");

        CobMeta v = PicEncoder.ToCobMeta(-123, pic);

        Assert.IsTrue(v.Number?.IsNegative);
        Assert.AreEqual("123", v.Number?.Digits);
        Assert.AreEqual(0, v.Number?.DecimalDigits);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("9(5)V9(2)", "12.3", "1230", 2)]
    public void Decimal(string picString, string value, string expected, int expectedScale)
    {
        decimal _value = decimal.Parse(value, CultureInfo.InvariantCulture);

        var pic = PicMeta.Parse(picString);

        CobMeta v = PicEncoder.ToCobMeta(_value, pic);

        Assert.IsFalse(v.Number?.IsNegative);
        Assert.AreEqual(expected, v.Number?.Digits);
        Assert.AreEqual(expectedScale, v.Number?.DecimalDigits);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("9(5)V9(2)", "-12.3",  "1230", 2)]
    [DataRow("9(1)V9(3)", "-12.3", "12300", 3)]
    public void Decimal_Is_Negative(string picString, string value, string expected, int expectedScale)
    {
        decimal _value = decimal.Parse(value, CultureInfo.InvariantCulture);

        var pic = PicMeta.Parse(picString);

        CobMeta v = PicEncoder.ToCobMeta(_value, pic);

        Assert.IsTrue(v.Number?.IsNegative);
        Assert.AreEqual(expected, v.Number?.Digits);
        Assert.AreEqual(expectedScale, v.Number?.DecimalDigits);
    }

    [TestMethod]
    public void DateOnly_Gregorian8_ToLogicalText()
    {
        var pic = PicMeta.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;
        
        var date = new DateOnly(2026, 1, 5);

        CobMeta v = PicEncoder.ToCobMeta(date, pic);

        Assert.IsFalse(v.Number?.IsNegative);
        Assert.AreEqual("20260105", v.Number?.Digits);
        Assert.AreEqual(0, v.Number?.DecimalDigits);
    }

    [TestMethod]
    public void DateOnly_Minguo7_ToLogicalText()
    {
        var pic = PicMeta.Parse("9(8)");
        pic.Semantic = PicSemantic.MinguoDate;
        
        var date = new DateOnly(2026, 1, 5);

        CobMeta v = PicEncoder.ToCobMeta(date, pic);

        Assert.IsFalse(v.Number?.IsNegative);
        Assert.AreEqual("1150105", v.Number?.Digits);
        Assert.AreEqual(0, v.Number?.DecimalDigits);
    }

    [TestMethod]
    public void TimeOnly_Time6_PadsZero()
    {
        var pic = PicMeta.Parse("9(6)");
        pic.Semantic = PicSemantic.Time6;

        var t = new TimeOnly(1, 2, 3);

        CobMeta v = PicEncoder.ToCobMeta(t, pic);

        Assert.IsFalse(v.Number?.IsNegative);
        Assert.AreEqual("010203", v.Number?.Digits);
        Assert.AreEqual(0, v.Number?.DecimalDigits);
    }

    [TestMethod]
    public void TimeOnly_Time9_MillisecondPadding()
    {
        var pic = PicMeta.Parse("9(9)");
        pic.Semantic = PicSemantic.Time9;

        var t = new TimeOnly(23, 59, 59, 7);

        CobMeta v = PicEncoder.ToCobMeta(t, pic);

        Assert.IsFalse(v.Number?.IsNegative);
        Assert.AreEqual("235959007", v.Number?.Digits);
        Assert.AreEqual(0, v.Number?.DecimalDigits);
    }

    [TestMethod]
    public void DateTime_Timestamp14()
    {
        var pic = PicMeta.Parse("9(14)");
        pic.Semantic = PicSemantic.Timestamp14;

        var dt = new DateTime(2025, 1, 6, 13, 45, 59);

        CobMeta v = PicEncoder.ToCobMeta(dt, pic);

        Assert.IsFalse(v.Number?.IsNegative);
        Assert.AreEqual("20250106134559", v.Number?.Digits);
        Assert.AreEqual(0, v.Number?.DecimalDigits);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    public void DateTime_Reject_Time6()
    {
        var pic = PicMeta.Parse("9(6)");
        pic.Semantic = PicSemantic.Time6;

        Assert.ThrowsException<NotSupportedException>(() => PicEncoder.ToCobMeta(new DateTime(), pic));
    }

    [TestMethod]
    public void DateTime_Reject_Date_Gregorian8()
    {
        var pic = PicMeta.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        Assert.ThrowsException<NotSupportedException>(() => PicEncoder.ToCobMeta(new DateTime(), pic));
    }

    [TestMethod]
    public void Decimal_Has_Fraction_But_Expected_Integer()
    {
        var pic = PicMeta.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        Assert.ThrowsException<InvalidOperationException>(() => PicEncoder.ToCobMeta(12345.6m, pic));
    }

    // Unsupported type
    [TestMethod]
    public void UnsupportedType_Throws()
    {
        Assert.ThrowsException<NotSupportedException>(() =>
            PicEncoder.ToCobMeta(new object(), PicMeta.Parse("X(5)")));
    }
}
