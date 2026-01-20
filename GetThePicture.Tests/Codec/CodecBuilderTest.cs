using GetThePicture.Cobol.Picture.TypeBase;

using GetThePicture.Codec;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Tests.Codec;

[TestClass]
public class CobolValueCodecTests
{
    [TestMethod]
    public void Encode_COMP_3()
    {
        var pic = Pic.Parse("9(5)");

        byte[] buffer = CodecBuilder.ForPic(pic)
            .WithUsage(PicUsage.PackedDecimal)
            .WithStrict()
            .Encode(52194);

        CollectionAssert.AreEqual(new byte[] { 0x52, 0x19, 0x4F }, buffer);
    }

    [TestMethod]
    public void Encode_With_Sign_COMP_3()
    {
        var pic = Pic.Parse("S9(5)");
        pic.Usage = PicUsage.PackedDecimal;

        byte[] buffer = CodecBuilder.ForPic(pic).WithStrict().Encode(52194);

        CollectionAssert.AreEqual(new byte[] { 0x52, 0x19, 0x4C }, buffer);
    }

    [TestMethod]
    public void Encode_With_Sign_COMP_3_Negative()
    {
        var pic = Pic.Parse("S9(5)");
        pic.Usage = PicUsage.PackedDecimal;

        byte[] buffer = CodecBuilder.ForPic(pic).WithStrict().Encode(-52194);

        CollectionAssert.AreEqual(new byte[] { 0x52, 0x19, 0x4D }, buffer);
    }

    [TestMethod]
    public void Encode_With_Sign_COMP_3_Lesser()
    {
        var pic = Pic.Parse("S9(3)");
        pic.Usage = PicUsage.PackedDecimal;

        byte[] buffer = CodecBuilder.ForPic(pic).Encode(-52194);

        CollectionAssert.AreEqual(new byte[] { 0x19, 0x4D }, buffer);
    }
    
    /// 
    /// 
    /// 

    [TestMethod]
    public void Decode_COMP_3()
    {
        var pic = Pic.Parse("9(5)");

        var value = CodecBuilder.ForPic(pic)
            .WithUsage(PicUsage.PackedDecimal)
            .WithStrict()
            .Decode([0x52, 0x19, 0x4C]);

        Assert.AreEqual(52194UL, value);
    }

    [TestMethod]
    public void Decode_With_Sign_COMP_3()
    {
        var pic = Pic.Parse("S9(5)");
        pic.Usage = PicUsage.PackedDecimal;

        var value = CodecBuilder.ForPic(pic).WithStrict().Decode([0x52, 0x19, 0x4C]);

        Assert.AreEqual(52194L, value);
    }

    [TestMethod]
    public void Decode_With_Sign_COMP_3_Negative()
    {
        var pic = Pic.Parse("S9(5)");
        pic.Usage = PicUsage.PackedDecimal;

        var value = CodecBuilder.ForPic(pic).WithStrict().Decode([0x52, 0x19, 0x4D]);

        Assert.AreEqual(-52194L, value);
    }

    [TestMethod]
    public void Decode_With_Sign_COMP_3_Lesser()
    {
        var pic = Pic.Parse("S9(3)");
        pic.Usage = PicUsage.PackedDecimal;

        var value = CodecBuilder.ForPic(pic).Decode([0x52, 0x19, 0x4D]);

        Assert.AreEqual(-194L, value);
    }

    [TestMethod]
    [ExpectedException(typeof(OverflowException))]
    public void Decode_COMP_3_ThrowsOverflowException()
    {
        var pic = Pic.Parse("9(5)");
        pic.Usage = PicUsage.PackedDecimal;
        
        CodecBuilder.ForPic(pic).Decode([0x52, 0x19, 0x4D]);
    }
}