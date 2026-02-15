using System.Globalization;
using System.Text;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Computational;
using GetThePicture.Picture.Clause.Codec.Category.Numeric;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Base.Computational;

[TestClass]
public class PackedDecimalTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [DataTestMethod]
    [DataRow("S9(5)", new byte[] { 0x12, 0x34, 0x5C }                  ,        12345,    typeof(int))]
    [DataRow("S9(5)", new byte[] { 0x12, 0x34, 0x5D }                  ,       -12345,    typeof(int))]
    [DataRow("S9(5)", new byte[] { 0x98, 0x76, 0x5D }                  ,       -98765,    typeof(int))]
    [DataRow("S9(5)", new byte[] { 0x00, 0x00, 0x1C }                  ,            1,    typeof(int))]
    [DataRow("S9(3)", new byte[] { 0x00, 0x0C }                        , (short)    0,  typeof(short))] // 邊界測試
    [DataRow("9(05)", new byte[] { 0x12, 0x34, 0x5F }                  , (uint) 12345,   typeof(uint))]
    [DataRow("9(10)", new byte[] { 0x00, 0x00, 0x00, 0x00, 0x00, 0x1F }, (ulong)    1,  typeof(ulong))]
    [DataRow("9(03)", new byte[] { 0x00, 0x0F }                        , (ushort) 0UL, typeof(ushort))] // 邊界測試
    public void Decode_Integer(string picText, byte[] buffer, object expected, Type expectedType)
    {
        var pic = PicMeta.Parse(picText);
        pic.Usage = PicUsage.PackedDecimal;

        object value = COMP3.Decode(buffer, pic);

        Assert.IsInstanceOfType(value, expectedType);
        Assert.AreEqual(expected, value);
    }

    [DataTestMethod]
    [DataRow("S9(5)V9(2)", new byte[] { 0x12, 0x34, 0x56, 0x7C }, "12345.67")]
    [DataRow("S9(3)V9(2)", new byte[] { 0x12, 0x34, 0x5D }      ,  "-123.45")]
    public void Decode_Decimal(string picText, byte[] buffer, string expectedValue)
    {
        var pic = PicMeta.Parse(picText);
        pic.Usage = PicUsage.PackedDecimal;

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
        var nMeta = new NumericMeta(cp950.GetBytes("12345"), 0, false);

        byte[] buffer = COMP3.Encode(nMeta, pic);

        // 12 34 5F
        CollectionAssert.AreEqual(new byte[] { 0x12, 0x34, 0x5F }, buffer);
    }
}