using System.Globalization;
using System.Text;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Encoder.Category.Numeric;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Encoder.Category.Numeric;

[TestClass]
public class MetaTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    public void Integer_PreservesSign()
    {
        var pic = PicMeta.Parse("9(5)");

        var v = NumericMeta.Parse(-123, pic);

        string actual = cp950.GetString(v.Chars);

        Assert.AreEqual("00123", actual);
        Assert.AreEqual(0, v.DecimalDigits);
        Assert.IsTrue(v.IsNegative);
    }

    [DataTestMethod]
    [DataRow("9(5)V9(2)", "12.3", "0001230", 2)]
    public void Decimal(string picString, string value, string expected, int expectedScale)
    {
        decimal _value = decimal.Parse(value, CultureInfo.InvariantCulture);

        var pic = PicMeta.Parse(picString);

        var v = NumericMeta.Parse(_value, pic);

        string actual = cp950.GetString(v.Chars);

        Assert.AreEqual(expected, actual);
        Assert.AreEqual(expectedScale, v.DecimalDigits);
        Assert.IsFalse(v.IsNegative);
    }

    [DataTestMethod]
    [DataRow("9(5)V9(2)", "-12.3", "0001230", 2)]
    [DataRow("9(2)V9(3)", "-12.3",   "12300", 3)]
    public void Decimal_Is_Negative(string picString, string value, string expected, int expectedScale)
    {
        decimal _value = decimal.Parse(value, CultureInfo.InvariantCulture);

        var pic = PicMeta.Parse(picString);

        var v = NumericMeta.Parse(_value, pic);

        string actual = cp950.GetString(v.Chars);

        Assert.AreEqual(expected, actual);
        Assert.AreEqual(expectedScale, v.DecimalDigits);
        Assert.IsTrue(v.IsNegative);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    public void DateTime_Reject_Time6()
    {
        var pic = PicMeta.Parse("9(6)");
        pic.Semantic = PicSemantic.Time6;

        Assert.ThrowsException<NotSupportedException>(() => NumericMeta.Parse(new DateTime(), pic));
    }

    [TestMethod]
    public void DateTime_Reject_Date_Gregorian8()
    {
        var pic = PicMeta.Parse("9(8)");
        pic.Semantic = PicSemantic.GregorianDate;

        Assert.ThrowsException<NotSupportedException>(() => NumericMeta.Parse(new DateTime(), pic));
    }

    // Unsupported type
    [TestMethod]
    public void UnsupportedType_Throws()
    {
        Assert.ThrowsException<NotSupportedException>(() =>
            NumericMeta.Parse(new object(), PicMeta.Parse("X(5)")));
    }
}
