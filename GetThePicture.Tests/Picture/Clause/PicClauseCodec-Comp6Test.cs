using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Tests.Picture.Clause;

[TestClass]
public class CodecComp6Test
{
    // -------------------------
    // Encode
    // ------------------------- 

    [DataTestMethod]
    [DataRow("9(1)", (byte)         5, new byte[] { 0x05 })]
    [DataRow("9(2)", (byte)        12, new byte[] { 0x12 })]
    [DataRow("9(3)", (ushort)     123, new byte[] { 0x01, 0x23 })]
    [DataRow("9(4)", (ushort)    1234, new byte[] { 0x12, 0x34 })]
    [DataRow("9(5)", (uint)     52194, new byte[] { 0x05, 0x21, 0x94 })]
    [DataRow("9(6)", (uint)    987654, new byte[] { 0x98, 0x76, 0x54 })]
    [DataRow("9(7)", (uint)   1234567, new byte[] { 0x01, 0x23, 0x45, 0x67 })]
    [DataRow("9(8)", (uint)  87654321, new byte[] { 0x87, 0x65, 0x43, 0x21 })]
    [DataRow("9(9)", (uint) 123456789, new byte[] { 0x01, 0x23, 0x45, 0x67, 0x89 })]
    public void Encode_Combination_Test(string picString, object value, byte[] expected)
    {
        var pic = PicMeta.Parse(picString);

        byte[] buffer = PicClauseCodec.ForMeta(pic)
            .Usage(PicUsage.UPackedDecimal)
            .WithStrict()
            .Encode(value);

        CollectionAssert.AreEqual(expected, buffer);
    }

    [TestMethod]
    public void Encode_With_Sign_Lesser()
    {
        var pic = PicMeta.Parse("9(3)");
        pic.Usage = PicUsage.UPackedDecimal;

        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(52194);

        CollectionAssert.AreEqual(new byte[] { 0x01, 0x94 }, buffer);
    }
    
    // -------------------------
    // Decode
    // ------------------------- 

    [DataTestMethod]
    [DataRow("9(1)", (byte)         5, new byte[] { 0x05 })]
    [DataRow("9(2)", (byte)        12, new byte[] { 0x12 })]
    [DataRow("9(3)", (ushort)     123, new byte[] { 0x01, 0x23 })]
    [DataRow("9(4)", (ushort)    1234, new byte[] { 0x12, 0x34 })]
    [DataRow("9(5)", (uint)     52194, new byte[] { 0x05, 0x21, 0x94 })]
    [DataRow("9(6)", (uint)    987654, new byte[] { 0x98, 0x76, 0x54 })]
    [DataRow("9(7)", (uint)   1234567, new byte[] { 0x01, 0x23, 0x45, 0x67 })]
    [DataRow("9(8)", (uint)  87654321, new byte[] { 0x87, 0x65, 0x43, 0x21 })]
    [DataRow("9(9)", (uint) 123456789, new byte[] { 0x01, 0x23, 0x45, 0x67, 0x89 })]
    public void Decode_Combination_Test(string picString, object expected, byte[] buffer)
    {
        var pic = PicMeta.Parse(picString);

        var value = PicClauseCodec.ForMeta(pic)
            .Usage(PicUsage.UPackedDecimal)
            .WithStrict()
            .Decode(buffer);

        Assert.AreEqual(expected, value);
    }

    [TestMethod]
    public void Decode_With_Sign_Lesser()
    {
        var pic = PicMeta.Parse("9(3)");
        pic.Usage = PicUsage.UPackedDecimal;

        var value = PicClauseCodec.ForMeta(pic).Decode([0x05, 0x21, 0x94]);

        Assert.AreEqual((ushort) 194, value);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(NotSupportedException))]
    public void Decode_With_Sign_Negative_ThrowsOverflowException()
    {
        var pic = PicMeta.Parse("S9(5)");
        pic.Usage = PicUsage.UPackedDecimal;
        
        PicClauseCodec.ForMeta(pic).Decode([0x05, 0x21, 0x94]);
    }
}