using System.Globalization;
using System.Text;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Encoder;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Encoder;

[TestClass]
public class NumericValueTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [DataTestMethod]
    [DataRow("X(5)", "Hello",  "Hello")]
    [DataRow("X(1)", "A",  "A")]
    public void StringChar_PassThrough(string picString, string value, string expected)
    {
        var pic = PicMeta.Parse(picString);

        var v = PicEncoder.EncodeNumeric(value, pic);

        string actual = cp950.GetString(v.Magnitude.Span);

        Assert.AreEqual(expected, actual);
    }
    
    [TestMethod]
    public void Integer_PreservesSign()
    {
        var pic = PicMeta.Parse("9(5)");

        var v = PicEncoder.EncodeNumeric(-123, pic);

        string actual = cp950.GetString(v.Magnitude.Span);

        Assert.IsTrue(v.IsNegative);
        Assert.AreEqual("00123", actual);
        Assert.AreEqual(0, v.DecimalDigits);
    }

    [DataTestMethod]
    [DataRow("9(5)V9(2)", "12.3", "0001230", 2)]
    public void Decimal(string picString, string value, string expected, int expectedScale)
    {
        decimal _value = decimal.Parse(value, CultureInfo.InvariantCulture);

        var pic = PicMeta.Parse(picString);

        var v = PicEncoder.EncodeNumeric(_value, pic);

        string actual = cp950.GetString(v.Magnitude.Span);

        Assert.IsFalse(v.IsNegative);
        Assert.AreEqual(expected, actual);
        Assert.AreEqual(expectedScale, v.DecimalDigits);
    }

    [DataTestMethod]
    [DataRow("9(5)V9(2)", "-12.3", "0001230", 2)]
    [DataRow("9(2)V9(3)", "-12.3",   "12300", 3)]
    public void Decimal_Is_Negative(string picString, string value, string expected, int expectedScale)
    {
        decimal _value = decimal.Parse(value, CultureInfo.InvariantCulture);

        var pic = PicMeta.Parse(picString);

        var v = PicEncoder.EncodeNumeric(_value, pic);

        string actual = cp950.GetString(v.Magnitude.Span);

        Assert.IsTrue(v.IsNegative);
        Assert.AreEqual(expected, actual);
        Assert.AreEqual(expectedScale, v.DecimalDigits);
    }

    [TestMethod]
    public void DateOnly_Gregorian8_ToLogicalText()
    {
        var pic = PicMeta.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;
        
        var date = new DateOnly(2026, 1, 5);

        var v = PicEncoder.EncodeNumeric(date, pic);

        string actual = cp950.GetString(v.Magnitude.Span);

        Assert.IsFalse(v.IsNegative);
        Assert.AreEqual("20260105", actual);
        Assert.AreEqual(0, v.DecimalDigits);
    }

    [TestMethod]
    public void DateOnly_Minguo7_ToLogicalText()
    {
        var pic = PicMeta.Parse("9(8)");
        pic.Semantic = PicSemantic.MinguoDate;
        
        var date = new DateOnly(2026, 1, 5);

        var v = PicEncoder.EncodeNumeric(date, pic);

        string actual = cp950.GetString(v.Magnitude.Span);

        Assert.IsFalse(v.IsNegative);
        Assert.AreEqual("1150105", actual);
        Assert.AreEqual(0, v.DecimalDigits);
    }

    [TestMethod]
    public void TimeOnly_Time6_PadsZero()
    {
        var pic = PicMeta.Parse("9(6)");
        pic.Semantic = PicSemantic.Time6;

        var t = new TimeOnly(1, 2, 3);

        var v = PicEncoder.EncodeNumeric(t, pic);

        string actual = cp950.GetString(v.Magnitude.Span);

        Assert.IsFalse(v.IsNegative);
        Assert.AreEqual("010203", actual);
        Assert.AreEqual(0, v.DecimalDigits);
    }

    [TestMethod]
    public void TimeOnly_Time9_MillisecondPadding()
    {
        var pic = PicMeta.Parse("9(9)");
        pic.Semantic = PicSemantic.Time9;

        var t = new TimeOnly(23, 59, 59, 7);

        var v = PicEncoder.EncodeNumeric(t, pic);

        string actual = cp950.GetString(v.Magnitude.Span);

        Assert.IsFalse(v.IsNegative);
        Assert.AreEqual("235959007", actual);
        Assert.AreEqual(0, v.DecimalDigits);
    }

    [TestMethod]
    public void DateTime_Timestamp14()
    {
        var pic = PicMeta.Parse("9(14)");
        pic.Semantic = PicSemantic.Timestamp14;

        var dt = new DateTime(2025, 1, 6, 13, 45, 59);

        var v = PicEncoder.EncodeNumeric(dt, pic);

        string actual = cp950.GetString(v.Magnitude.Span);

        Assert.IsFalse(v.IsNegative);
        Assert.AreEqual("20250106134559", actual);
        Assert.AreEqual(0, v.DecimalDigits);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    public void DateTime_Reject_Time6()
    {
        var pic = PicMeta.Parse("9(6)");
        pic.Semantic = PicSemantic.Time6;

        Assert.ThrowsException<NotSupportedException>(() => PicEncoder.EncodeNumeric(new DateTime(), pic));
    }

    [TestMethod]
    public void DateTime_Reject_Date_Gregorian8()
    {
        var pic = PicMeta.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        Assert.ThrowsException<NotSupportedException>(() => PicEncoder.EncodeNumeric(new DateTime(), pic));
    }

    [TestMethod]
    public void Decimal_Has_Fraction_But_Expected_Integer()
    {
        var pic = PicMeta.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        Assert.ThrowsException<InvalidOperationException>(() => PicEncoder.EncodeNumeric(12345.6m, pic));
    }

    // Unsupported type
    [TestMethod]
    public void UnsupportedType_Throws()
    {
        Assert.ThrowsException<NotSupportedException>(() =>
            PicEncoder.EncodeNumeric(new object(), PicMeta.Parse("X(5)")));
    }
}
