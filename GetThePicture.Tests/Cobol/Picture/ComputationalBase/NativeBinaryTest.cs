using GetThePicture.Cobol.Elementary;
using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Cobol.Picture.ComputationalBase;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Tests.Cobol.Picture.ComputationalBase;

[TestClass]
public class NativeBinaryTest
{
    private static Type GetExpectedType(PicClause pic)
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
    
    [TestMethod]
    [DataTestMethod]
    [DataRow("9(4)", false, "0", (ushort) 0)]
    [DataRow("9(4)", false, "65535", ushort.MaxValue)]
    public void Codec_Halfword(string picString, bool isNegative, string digits, ushort expected)
    {
        var pic = Pic.Parse(picString);
        
        var displayValue = ElementaryMeta.FromNumber(isNegative, digits, decimalDigits: 0);
        
        // Encode
        byte[] bytesLE = COMP5.Encode(displayValue, pic, BinaryOptions.Normal); // x86/x86-64
        byte[] bytesBE = COMP5.Encode(displayValue, pic, BinaryOptions.Reversed);

        // Decode
        object decodedLE = COMP5.Decode(bytesLE, pic, BinaryOptions.Normal); // x86/x86-64
        object decodedBE = COMP5.Decode(bytesBE, pic, BinaryOptions.Reversed);

        // 檢查型別
        Type expectedType = GetExpectedType(pic);
        Assert.IsInstanceOfType(decodedLE, expectedType, $"LE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.IsInstanceOfType(decodedBE, expectedType, $"BE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");

        // 檢查值
        Assert.AreEqual(expected, Convert.ToUInt16(decodedLE), $"LE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.AreEqual(expected, Convert.ToUInt16(decodedBE), $"BE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("S9(4)", false, "32767", short.MaxValue)]
    [DataRow("S9(4)",  true, "32768", short.MinValue)]
    public void Codec_Sign_Halfword(string picString, bool isNegative, string digits, short expected)
    {
        var pic = Pic.Parse(picString);
        
        var displayValue = ElementaryMeta.FromNumber(isNegative, digits, decimalDigits: 0);
        
        // Encode
        byte[] bytesLE = COMP5.Encode(displayValue, pic, BinaryOptions.Normal); // x86/x86-64
        byte[] bytesBE = COMP5.Encode(displayValue, pic, BinaryOptions.Reversed);

        // Decode
        object decodedLE = COMP5.Decode(bytesLE, pic, BinaryOptions.Normal); // x86/x86-64
        object decodedBE = COMP5.Decode(bytesBE, pic, BinaryOptions.Reversed);

        // 檢查型別
        Type expectedType = GetExpectedType(pic);
        Assert.IsInstanceOfType(decodedLE, expectedType, $"LE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.IsInstanceOfType(decodedBE, expectedType, $"BE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");

        // 檢查值
        Assert.AreEqual(expected, Convert.ToInt16(decodedLE), $"LE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.AreEqual(expected, Convert.ToInt16(decodedBE), $"BE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("9(9)", false, "0", (uint) 0)]
    [DataRow("9(9)", false, "4294967295", uint.MaxValue)]
    public void Codec_Fullword(string picString, bool isNegative, string digits, uint expected)
    {
        var pic = Pic.Parse(picString);
        
        var displayValue = ElementaryMeta.FromNumber(isNegative, digits, decimalDigits: 0);
        
        // Encode
        byte[] bytesLE = COMP5.Encode(displayValue, pic, BinaryOptions.Normal); // x86/x86-64
        byte[] bytesBE = COMP5.Encode(displayValue, pic, BinaryOptions.Reversed);

        // Decode
        object decodedLE = COMP5.Decode(bytesLE, pic, BinaryOptions.Normal); // x86/x86-64
        object decodedBE = COMP5.Decode(bytesBE, pic, BinaryOptions.Reversed);

        // 檢查型別
        Type expectedType = GetExpectedType(pic);
        Assert.IsInstanceOfType(decodedLE, expectedType, $"LE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.IsInstanceOfType(decodedBE, expectedType, $"BE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");

        // 檢查值
        Assert.AreEqual(expected, Convert.ToUInt32(decodedLE), $"LE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.AreEqual(expected, Convert.ToUInt32(decodedBE), $"BE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("S9(9)", false, "2147483647", int.MaxValue)]
    [DataRow("S9(9)",  true, "2147483648", int.MinValue)]
    public void Codec_Sign_Fullword(string picString, bool isNegative, string digits, int expected)
    {
        var pic = Pic.Parse(picString);
        
        var displayValue = ElementaryMeta.FromNumber(isNegative, digits, decimalDigits: 0);
        
        // Encode
        byte[] bytesLE = COMP5.Encode(displayValue, pic, BinaryOptions.Normal); // x86/x86-64
        byte[] bytesBE = COMP5.Encode(displayValue, pic, BinaryOptions.Reversed);

        // Decode
        object decodedLE = COMP5.Decode(bytesLE, pic, BinaryOptions.Normal); // x86/x86-64
        object decodedBE = COMP5.Decode(bytesBE, pic, BinaryOptions.Reversed);

        // 檢查型別
        Type expectedType = GetExpectedType(pic);
        Assert.IsInstanceOfType(decodedLE, expectedType, $"LE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.IsInstanceOfType(decodedBE, expectedType, $"BE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");

        // 檢查值
        Assert.AreEqual(expected, Convert.ToInt32(decodedLE), $"LE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.AreEqual(expected, Convert.ToInt32(decodedBE), $"BE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("9(18)", false, "0", (ulong) 0)]
    [DataRow("9(18)", false, "18446744073709551615", ulong.MaxValue)]
    public void Codec_Doubleword(string picString, bool isNegative, string digits, ulong expected)
    {
        var pic = Pic.Parse(picString);
        
        var displayValue = ElementaryMeta.FromNumber(isNegative, digits, decimalDigits: 0);
        
        // Encode
        byte[] bytesLE = COMP5.Encode(displayValue, pic, BinaryOptions.Normal); // x86/x86-64
        byte[] bytesBE = COMP5.Encode(displayValue, pic, BinaryOptions.Reversed);

        // Decode
        object decodedLE = COMP5.Decode(bytesLE, pic, BinaryOptions.Normal); // x86/x86-64
        object decodedBE = COMP5.Decode(bytesBE, pic, BinaryOptions.Reversed);

        // 檢查型別
        Type expectedType = GetExpectedType(pic);
        Assert.IsInstanceOfType(decodedLE, expectedType, $"LE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.IsInstanceOfType(decodedBE, expectedType, $"BE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");

        // 檢查值
        Assert.AreEqual(expected, Convert.ToUInt64(decodedLE), $"LE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.AreEqual(expected, Convert.ToUInt64(decodedBE), $"BE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow("S9(18)", false, "9223372036854775807", long.MaxValue)]
    [DataRow("S9(18)",  true, "9223372036854775808", long.MinValue)]
    public void Codec_Sign_Doubleword(string picString, bool isNegative, string digits, long expected)
    {
        var pic = Pic.Parse(picString);
        
        var displayValue = ElementaryMeta.FromNumber(isNegative, digits, decimalDigits: 0);
        
        // Encode
        byte[] bytesLE = COMP5.Encode(displayValue, pic, BinaryOptions.Normal); // x86/x86-64
        byte[] bytesBE = COMP5.Encode(displayValue, pic, BinaryOptions.Reversed);

        // Decode
        object decodedLE = COMP5.Decode(bytesLE, pic, BinaryOptions.Normal); // x86/x86-64
        object decodedBE = COMP5.Decode(bytesBE, pic, BinaryOptions.Reversed);

        // 檢查型別
        Type expectedType = GetExpectedType(pic);
        Assert.IsInstanceOfType(decodedLE, expectedType, $"LE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.IsInstanceOfType(decodedBE, expectedType, $"BE Type mismatch for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");

        // 檢查值
        Assert.AreEqual(expected, Convert.ToInt64(decodedLE), $"LE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
        Assert.AreEqual(expected, Convert.ToInt64(decodedBE), $"BE failed for {expected}, {pic.DigitCount} bytes, Signed={pic.Signed}");
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    [DataTestMethod]
    [DataRow(  "9(4)", false, "65536")]
    [DataRow( "S9(4)", false, "32768")]
    [DataRow( "S9(9)", false, "2147483648")]
    [DataRow("S9(18)", false, "9223372036854775808")]
    public void Codec_Numeric_Overflow(string picString, bool isNegative, string digits)
    {
        var pic = Pic.Parse(picString);
        
        var displayValue = ElementaryMeta.FromNumber(isNegative, digits, decimalDigits: 0);

        Assert.ThrowsException<OverflowException>(() => COMP5.Encode(displayValue, pic));
    }

    [TestMethod]
    [DataTestMethod]
    [DataRow( "9(2)V99", false, "1")]
    [DataRow("S9(2)V99", false, "1")]
    public void Codec_Numeric_WithDecimal(string picString, bool isNegative, string digits)
    {
        var pic = Pic.Parse(picString);
        
        var displayValue = ElementaryMeta.FromNumber(isNegative, digits, decimalDigits: 0);

        Assert.ThrowsException<NotSupportedException>(() => COMP5.Encode(displayValue, pic));
    }
}