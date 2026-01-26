using System.Globalization;
using System.Text;

using GetThePicture.PictureClause;
using GetThePicture.PictureClause.Base;

namespace GetThePicture.Tests.PictureClause.Decoder.Category;

[TestClass]
public class NumericDecoderForIntegerTest
{
    /// <summary>
    /// 
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    private static bool IsIntegerValue(object value)
    {
        return value switch
        {
            decimal d => decimal.Truncate(d) == d,
            sbyte or byte or short or ushort or int or uint or long or ulong => true,
            _ => false
        };
    }
    
    [TestMethod]
    [DataTestMethod]
    [DataRow( "9",  "9(1)", typeof (byte),  (byte)  9)]
    [DataRow( "I", "S9(1)", typeof (byte),  (byte)  9)]
    [DataRow( "R", "S9(1)", typeof(sbyte), (sbyte) -9)]
    [DataRow("99",  "9(2)", typeof (byte),  (byte) 99)]
    [DataRow("9I", "S9(2)", typeof (byte),  (byte) 99)]
    [DataRow("9R", "S9(2)", typeof(sbyte), (sbyte)-99)]
    public void Decode_Default_Byte(string text, string picString, Type expectedType, object expectedValue)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(expectedValue, value);
        Assert.IsTrue(IsIntegerValue(value));
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow( "998",  "9(3)", typeof(ushort), (ushort)  998)]
    [DataRow( "99H", "S9(3)", typeof(ushort), (ushort)  998)]
    [DataRow( "99Q", "S9(3)", typeof (short),  (short) -998)]
    [DataRow("9998",  "9(4)", typeof(ushort), (ushort) 9998)]
    [DataRow("999H", "S9(4)", typeof(ushort), (ushort) 9998)]
    [DataRow("999Q", "S9(4)", typeof (short),  (short)-9998)]
    public void Decode_Default_Short(string text, string picString, Type expectedType, object expectedValue)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(expectedValue, value);
        Assert.IsTrue(IsIntegerValue(value));
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow(    "99997",  "9(5)", typeof(uint), (uint)     99997)]
    [DataRow(    "9999G", "S9(5)", typeof(uint), (uint)     99997)]
    [DataRow(    "9999P", "S9(5)", typeof (int),  (int)    -99997)]
    [DataRow("999999997",  "9(9)", typeof(uint), (uint) 999999997)]
    [DataRow("99999999G", "S9(9)", typeof(uint), (uint) 999999997)]
    [DataRow("99999999P", "S9(9)", typeof (int),  (int)-999999997)]
    public void Decode_Default_Int(string text, string picString, Type expectedType, object expectedValue)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(expectedValue, value);
        Assert.IsTrue(IsIntegerValue(value));
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow(        "9999999996",  "9(10)", typeof(ulong), (ulong)         9999999996)]
    [DataRow(        "999999999F", "S9(10)", typeof(ulong), (ulong)         9999999996)]
    [DataRow(        "999999999O", "S9(10)", typeof (long),  (long)        -9999999996)]
    [DataRow("999999999999999996",  "9(18)", typeof(ulong), (ulong) 999999999999999996)]
    [DataRow("99999999999999999F", "S9(18)", typeof(ulong), (ulong) 999999999999999996)]
    [DataRow("99999999999999999O", "S9(18)", typeof (long),  (long)-999999999999999996)]
    public void Decode_Default_Long(string text, string picString, Type expectedType, object expectedValue)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(expectedValue, value);
        Assert.IsTrue(IsIntegerValue(value));
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow(         "9999999999999999995",  "9(19)", typeof(decimal),           "9999999999999999995")]
    [DataRow(         "999999999999999999E", "S9(19)", typeof(decimal),           "9999999999999999995")]
    [DataRow(         "999999999999999999N", "S9(19)", typeof(decimal),          "-9999999999999999995")]
    [DataRow("9999999999999999999999999995",  "9(28)", typeof(decimal),  "9999999999999999999999999995")]
    [DataRow("999999999999999999999999999E", "S9(28)", typeof(decimal),  "9999999999999999999999999995")]
    [DataRow("999999999999999999999999999N", "S9(28)", typeof(decimal), "-9999999999999999999999999995")]
    public void Decode_Default_DecimalWithScaleZero(string text, string picString, Type expectedType, string expectedValue)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(decimal.Parse(expectedValue, CultureInfo.InvariantCulture), value);
        Assert.IsTrue(IsIntegerValue(value));
    }

    // -------------------------
    // SIGN IS LEADING
    // -------------------------

    [TestMethod]
    public void Decode_With_LeadingSign()
    {
        var pic = PicMeta.Parse("S9(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("}0123");

        object value = PicClauseCodec.ForMeta(pic).WithSignIsLeading().Decode(buffer);

        Assert.IsInstanceOfType(value, typeof(int));
        Assert.AreEqual(-123, value);
        Assert.IsTrue(IsIntegerValue(value));
    }

    // -------------------------
    // Invalid format
    // -------------------------

    [TestMethod]
    [DataTestMethod]
    [DataRow("99999999999999999999999999994",  "9(29)", typeof(decimal),  "99999999999999999999999999994")]
    [DataRow("9999999999999999999999999999D", "S9(29)", typeof(decimal),  "99999999999999999999999999994")]
    [DataRow("9999999999999999999999999999M", "S9(29)", typeof(decimal), "-99999999999999999999999999994")]
    [ExpectedException(typeof(OverflowException))]
    public void Decode_ThrowsOverflowException(string text, string picString, Type expectedType, string expectedValue)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = Encoding.ASCII.GetBytes(text);

        object value = PicClauseCodec.ForMeta(pic).Decode(buffer);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(decimal.Parse(expectedValue, CultureInfo.InvariantCulture), value);
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_NumericWithNonDigit_ThrowsFormatException()
    {
        var pic = PicMeta.Parse("9(5)");
        byte[] buffer = Encoding.ASCII.GetBytes("12A34");

        PicClauseCodec.ForMeta(pic).Decode(buffer);
    }
}