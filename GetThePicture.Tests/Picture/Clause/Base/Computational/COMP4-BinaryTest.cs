using System.Text;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.Computational;
using GetThePicture.Picture.Clause.Codec.Category.Numeric;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Base.Computational;

[TestClass]
public class BinaryTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    private static Type GetExpectedType(PicMeta pic)
    {
        int length = pic.DigitCount switch
        {
            <=  4 => 2,
            <=  9 => 4,
            <= 18 => 8,
            _ => throw new NotSupportedException()
        };

        return length switch
        {
            2 => pic.Signed ? typeof(short) : typeof(ushort),
            4 => pic.Signed ? typeof(int)   : typeof(uint),
            8 => pic.Signed ? typeof(long)  : typeof(ulong),
            _ => throw new NotSupportedException()
        };
    }

    [DataTestMethod]
    [DataRow("9(4)", false,     "0", (ushort)       0, new byte[] { 0x00, 0x00 })]
    [DataRow("9(4)", false,     "1", (ushort)       1, new byte[] { 0x00, 0x01 })]
    [DataRow("9(4)", false,   "256", (ushort)     256, new byte[] { 0x01, 0x00 })]
    [DataRow("9(4)", false,  "4660", (ushort)    4660, new byte[] { 0x12, 0x34 })]
    [DataRow("9(4)", false, "65535",  ushort.MaxValue, new byte[] { 0xFF, 0xFF })]
    public void Codec_Halfword(string picString, bool isNegative, string digits, ushort expected, byte[] expectedBytes)
    {
        var pic = PicMeta.Parse(picString);
        
        var nMeta = new NumericMeta(cp950.GetBytes(digits), decimalDigits: 0, isNegative);
        
        // Encode
        byte[] bytesBE = COMP4.Encode(nMeta, pic);

        // 檢查 Big Endian bytes
        CollectionAssert.AreEqual(expectedBytes, bytesBE, $"BE bytes mismatch for {expected}");

        // Decode
        object decodedBE = COMP4.Decode(bytesBE, pic);

        // 檢查型別
        Type expectedType = GetExpectedType(pic);
        Assert.IsInstanceOfType(decodedBE, expectedType, $"BE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");

        // 檢查值
        Assert.AreEqual(expected, Convert.ToUInt16(decodedBE), $"BE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
    }

    [DataTestMethod]
    [DataRow("S9(4)", false, "0",        (short)0,        new byte[] { 0x00, 0x00 })]
    [DataRow("S9(4)", false, "1",        (short)1,        new byte[] { 0x00, 0x01 })]
    [DataRow("S9(4)",  true, "1",        (short)-1,       new byte[] { 0xFF, 0xFF })]
    [DataRow("S9(4)", false, "32767",    short.MaxValue,  new byte[] { 0x7F, 0xFF })]
    [DataRow("S9(4)",  true, "32768",    short.MinValue,  new byte[] { 0x80, 0x00 })]
    public void Codec_Sign_Halfword( string picString, bool isNegative, string digits, short expected, byte[] expectedBytes)
    {
        var pic = PicMeta.Parse(picString);
        
        var nMeta = new NumericMeta(cp950.GetBytes(digits), decimalDigits: 0, isNegative);

        // Encode
        byte[] bytesBE = COMP4.Encode(nMeta, pic);

        // 🔎 驗證 Big Endian byte layout
        CollectionAssert.AreEqual(
            expectedBytes,
            bytesBE,
            $"BE bytes mismatch for {expected}");

        // Decode
        object decodedBE = COMP4.Decode(bytesBE, pic);

        // 型別檢查
        Type expectedType = GetExpectedType(pic);
        Assert.IsInstanceOfType(
            decodedBE,
            expectedType,
            $"Type mismatch for {expected}");

        // 數值檢查
        Assert.AreEqual(
            expected,
            Convert.ToInt16(decodedBE),
            $"Value mismatch for {expected}");
    }
}