using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.PictureClause;

namespace GetThePicture.Tests.PictureClause;

[TestClass]
public class CompTest
{
    // -------------------------
    // Encode
    // ------------------------- 

    [TestMethod]
    [DataTestMethod]
    [DataRow( "9(04)", (ushort)  9999, new byte[] { 0x0F, 0x27 })] // 9999 >> 0x270F >> (Little Endian) >> 0F 27
    [DataRow("S9(04)",  (short)  9999, new byte[] { 0x0F, 0x27 })]
    [DataRow("S9(04)",  (short) -9999, new byte[] { 0xF1, 0xD8 })]
    [DataRow( "9(09)", (uint)  999999999, new byte[] { 0xFF, 0xC9, 0x9A, 0x3B })]
    [DataRow("S9(09)",  (int)  999999999, new byte[] { 0xFF, 0xC9, 0x9A, 0x3B })]
    [DataRow("S9(09)",  (int) -999999999, new byte[] { 0x01, 0x36, 0x65, 0xC4 })]
    [DataRow( "9(18)",  999999999999999999UL, new byte[] { 0xFF, 0xFF, 0x63, 0xA7, 0xB3, 0xB6, 0xE0, 0x0D })]
    [DataRow("S9(18)",  999999999999999999L , new byte[] { 0xFF, 0xFF, 0x63, 0xA7, 0xB3, 0xB6, 0xE0, 0x0D })]
    [DataRow("S9(18)", -999999999999999999L , new byte[] { 0x01, 0x00, 0x9C, 0x58, 0x4C, 0x49, 0x1F, 0xF2 })]
    public void Encode_Combination_Test(string picString, object value, byte[] expected)
    {
        var pic = PicMeta.Parse(picString);
        pic.Usage = PicUsage.Binary;

        byte[] buffer = PicClauseCodec.ForMeta(pic).WithStrict().Encode(value);

        CollectionAssert.AreEqual(expected, buffer);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("S9(04)", (short) 9999, new byte[] { 0x27, 0x0F })]
    [DataRow("S9(18)", -999999999999999999L, new byte[] { 0xF2, 0x1F, 0x49, 0x4C, 0x58, 0x9C, 0x00, 0x01 })]
    public void Encode_With_Change_Endian(string picString, object value, byte[] expected)
    {
        var pic = PicMeta.Parse(picString);
        pic.Usage = PicUsage.Binary;

        byte[] buffer = PicClauseCodec.ForMeta(pic).WithStrict().WithReversedBinary().Encode(value);

        CollectionAssert.AreEqual(expected, buffer);
    }
    
    // -------------------------
    // Decode
    // ------------------------- 

    [TestMethod]
    [DataTestMethod]
    [DataRow( "9(04)", (ushort)  9999, new byte[] { 0x0F, 0x27 })] // 9999 >> 0x270F >> (Little Endian) >> 0F 27
    [DataRow("S9(04)",  (short)  9999, new byte[] { 0x0F, 0x27 })]
    [DataRow("S9(04)",  (short) -9999, new byte[] { 0xF1, 0xD8 })]
    [DataRow( "9(09)", (uint)  999999999, new byte[] { 0xFF, 0xC9, 0x9A, 0x3B })]
    [DataRow("S9(09)",  (int)  999999999, new byte[] { 0xFF, 0xC9, 0x9A, 0x3B })]
    [DataRow("S9(09)",  (int) -999999999, new byte[] { 0x01, 0x36, 0x65, 0xC4 })]
    [DataRow( "9(18)",  999999999999999999UL, new byte[] { 0xFF, 0xFF, 0x63, 0xA7, 0xB3, 0xB6, 0xE0, 0x0D })]
    [DataRow("S9(18)",  999999999999999999L , new byte[] { 0xFF, 0xFF, 0x63, 0xA7, 0xB3, 0xB6, 0xE0, 0x0D })]
    [DataRow("S9(18)", -999999999999999999L , new byte[] { 0x01, 0x00, 0x9C, 0x58, 0x4C, 0x49, 0x1F, 0xF2 })]
    public void Decode_Combination_Test(string picString, object expected, byte[] buffer)
    {
        var pic = PicMeta.Parse(picString);
        pic.Usage = PicUsage.Binary;

        var value = PicClauseCodec.ForMeta(pic).WithStrict().Decode(buffer);

        Assert.AreEqual(expected, value);
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("S9(04)", (short) 9999, new byte[] { 0x27, 0x0F })]
    [DataRow("S9(18)", -999999999999999999L, new byte[] { 0xF2, 0x1F, 0x49, 0x4C, 0x58, 0x9C, 0x00, 0x01 })]
    public void Decode_With_Change_Endian(string picString, object expected, byte[] buffer)
    {
        var pic = PicMeta.Parse(picString);
        pic.Usage = PicUsage.Binary;

        var value = PicClauseCodec.ForMeta(pic).WithStrict().WithReversedBinary().Decode(buffer);

        Assert.AreEqual(expected, value);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(OverflowException))]
    public void Encode_Without_Sign_Negative_ThrowsOverflowException()
    {
        // PIC 9(4) COMP.
        var pic = PicMeta.Parse("9(4)");
        pic.Usage = PicUsage.Binary;

        PicClauseCodec.ForMeta(pic).WithStrict().Encode(-9999);
    }
}