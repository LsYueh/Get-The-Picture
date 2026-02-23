using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Tests.Picture.Clause.Codec;

[TestClass]
public class DefaultRepresentationTest
{
    [DataTestMethod]
    [DataRow( "A(5)", PicUsage.Display, new byte[] { (byte)' ', (byte)' ', (byte)' ', (byte)' ', (byte)' ' })]
    [DataRow( "X(5)", PicUsage.Display, new byte[] { (byte)' ', (byte)' ', (byte)' ', (byte)' ', (byte)' ' })]
    [DataRow( "9(5)", PicUsage.Display, new byte[] { (byte)'0', (byte)'0', (byte)'0', (byte)'0', (byte)'0' })]
    [DataRow("S9(5)", PicUsage.Display, new byte[] { (byte)'0', (byte)'0', (byte)'0', (byte)'0', (byte)'{' })]
    
    [DataRow( "9(5)", PicUsage.COMP3  , new byte[] { 0x00, 0x00, 0x0F })]
    [DataRow("S9(5)", PicUsage.COMP3  , new byte[] { 0x00, 0x00, 0x0C })]
    [DataRow( "9(6)", PicUsage.COMP3  , new byte[] { 0x00, 0x00, 0x00, 0x0F })]
    [DataRow("S9(6)", PicUsage.COMP3  , new byte[] { 0x00, 0x00, 0x00, 0x0C })]

    [DataRow( "9(5)", PicUsage.COMP4  , new byte[] { 0x00, 0x00, 0x00, 0x00 })]
    [DataRow("S9(5)", PicUsage.COMP4  , new byte[] { 0x00, 0x00, 0x00, 0x00 })]

    [DataRow( "9(5)", PicUsage.COMP5  , new byte[] { 0x00, 0x00, 0x00, 0x00 })]
    [DataRow("S9(5)", PicUsage.COMP5  , new byte[] { 0x00, 0x00, 0x00, 0x00 })]

    [DataRow( "9(5)", PicUsage.COMP6  , new byte[] { 0x00, 0x00, 0x00 })]
    [DataRow( "9(6)", PicUsage.COMP6  , new byte[] { 0x00, 0x00, 0x00 })]
    public void Create_Default_Representation_Test(string picString, PicUsage usage, byte[] expected)
    {
        var pic = PicMeta.Parse(picString);

         byte[] buffer = PicClauseCodec.ForMeta(pic)
            .Usage(usage)
            .WithStrict().CreateDefaultRepresentation();

        CollectionAssert.AreEqual(expected, buffer);
    }
}
