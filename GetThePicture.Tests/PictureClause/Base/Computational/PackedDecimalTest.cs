using System.Globalization;

using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.ClauseItems;
using GetThePicture.PictureClause.Base.Computational;
using GetThePicture.PictureClause.Encoder.Meta;

namespace GetThePicture.Tests.PictureClause.Base.Computational;

[TestClass]
public class PackedDecimalTest
{
    private static byte[] HexToBytes(string hex)
    {
        if (hex.Length % 2 != 0)
            throw new ArgumentException("Hex string length must be even");

        byte[] buffer = new byte[hex.Length / 2];

        for (int i = 0; i < buffer.Length; i++)
        {
            buffer[i] = Convert.ToByte(hex.Substring(i * 2, 2), 16);
        }

        return buffer;
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("S9(5)", "12345C",  12345L)]
    [DataRow("S9(5)", "12345D", -12345L)]
    [DataRow("S9(5)", "98765D", -98765L)]
    [DataRow("S9(5)", "00001C",      1L)]
    [DataRow("S9(3)",   "000C",      0L)] // 邊界測試
    public void Decode_Integer(string picText, string hex, long expected)
    {
        var pic = PicMeta.Parse(picText);
        pic.Usage = PicUsage.PackedDecimal;

        byte[] buffer = HexToBytes(hex);

        object value = COMP3.Decode(buffer, pic);

        Assert.IsInstanceOfType(value, typeof(long));
        Assert.AreEqual(expected, (long)value);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("9(05)",       "12345F", 12345UL)]
    [DataRow("9(10)", "00000000001F",     1UL)]
    [DataRow("9(03)",         "000F",     0UL)] // 邊界測試
    public void Decode_Unsigned_Integer(string picText, string hex, ulong expected)
    {
        var pic = PicMeta.Parse(picText);
        pic.Usage = PicUsage.PackedDecimal;

        byte[] buffer = HexToBytes(hex);

        object value = COMP3.Decode(buffer, pic);

        Assert.IsInstanceOfType(value, typeof(ulong));
        Assert.AreEqual(expected, (ulong)value);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("S9(5)V9(2)", "1234567C", "12345.67")]
    [DataRow("S9(3)V9(2)", "12345D"  ,  "-123.45")]
    public void Decode_Decimal(string picText, string hex, string expectedValue)
    {
        var pic = PicMeta.Parse(picText);
        pic.Usage = PicUsage.PackedDecimal;

        byte[] buffer = HexToBytes(hex);

        object value = COMP3.Decode(buffer, pic);

        Assert.AreEqual(decimal.Parse(expectedValue, CultureInfo.InvariantCulture), (decimal)value);
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_Invalid_Sign_Nibble_Should_Throw()
    {
        var pic = PicMeta.Parse("S9(3)");
        pic.Usage = PicUsage.PackedDecimal;

        // invalid sign = 0xA
        byte[] buffer = [0x12, 0x3A];

        COMP3.Decode(buffer, pic);
    }

    [TestMethod]
    public void Encode_Unsigned_Should_Use_F_Sign()
    {
        var pic = PicMeta.Parse("9(5)");
        CobMeta meta = CobMeta.FromNumber("12345");

        byte[] buffer = COMP3.Encode(meta, pic);

        // 12 34 5F
        CollectionAssert.AreEqual(HexToBytes("12345F"), buffer);
    }
}