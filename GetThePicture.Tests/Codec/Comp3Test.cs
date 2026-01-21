using GetThePicture.Cobol.Picture.TypeBase;

using GetThePicture.Codec;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Tests.Codec;

[TestClass]
public class Comp3Test
{
    // -------------------------
    // Encode
    // ------------------------- 

    [TestMethod]
    [DataTestMethod]
    [DataRow( "9(5)",  52194, new byte[] { 0x52, 0x19, 0x4F })]
    [DataRow("S9(5)",  52194, new byte[] { 0x52, 0x19, 0x4C })]
    [DataRow("S9(5)", -52194, new byte[] { 0x52, 0x19, 0x4D })]
    public void Encode_Combination_Test(string picString, object value, byte[] expected)
    {
        var pic = Pic.Parse(picString);

        byte[] buffer = CodecBuilder.ForPic(pic)
            .WithUsage(PicUsage.PackedDecimal)
            .WithStrict()
            .Encode(value);

        CollectionAssert.AreEqual(expected, buffer);
    }

    [TestMethod]
    public void Encode_With_Sign_Lesser()
    {
        var pic = Pic.Parse("S9(3)");
        pic.Usage = PicUsage.PackedDecimal;

        byte[] buffer = CodecBuilder.ForPic(pic).Encode(-52194);

        CollectionAssert.AreEqual(new byte[] { 0x19, 0x4D }, buffer);
    }
    
    // -------------------------
    // Decode
    // ------------------------- 

    [TestMethod]
    [DataTestMethod]
    [DataRow( "9(5)",  52194UL, new byte[] { 0x52, 0x19, 0x4F })]
    [DataRow( "9(5)",  52194UL, new byte[] { 0x52, 0x19, 0x4C })] // TODO: Sign Nibble 與 PIC 定義不合，看看要不要擋
    [DataRow("S9(5)",  52194L , new byte[] { 0x52, 0x19, 0x4C })]
    [DataRow("S9(5)", -52194L , new byte[] { 0x52, 0x19, 0x4D })]
    public void Decode_Combination_Test(string picString, object expected, byte[] buffer)
    {
        var pic = Pic.Parse(picString);

        var value = CodecBuilder.ForPic(pic)
            .WithUsage(PicUsage.PackedDecimal)
            .WithStrict()
            .Decode(buffer);

        Assert.AreEqual(expected, value);
    }

    [TestMethod]
    public void Decode_With_Sign_Lesser()
    {
        var pic = Pic.Parse("S9(3)");
        pic.Usage = PicUsage.PackedDecimal;

        var value = CodecBuilder.ForPic(pic).Decode([0x52, 0x19, 0x4D]);

        Assert.AreEqual(-194L, value);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(OverflowException))]
    public void Decode_Without_Sign_Negative_ThrowsOverflowException()
    {
        var pic = Pic.Parse("9(5)");
        pic.Usage = PicUsage.PackedDecimal;
        
        CodecBuilder.ForPic(pic).Decode([0x52, 0x19, 0x4D]);
    }
}