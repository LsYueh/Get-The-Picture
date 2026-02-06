using System.Text;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Encoder.Category;

[TestClass]
public class NumericEncoderTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [DataTestMethod]
    [DataRow(123.45, "9(3)V9(2)", "12345")]
    [DataRow(123.45, "9(3)V9(5)", "12345000")]
    [DataRow(123.45, "9(2)V9(2)",  "2345")]
    [DataRow( 12.3 , "9(1)V9(3)",  "2300")]
    public void Encode_Double_Default(object value, string picString, string expected)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(value);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }

    [DataTestMethod]
    [DataRow(123.45, "S9(3)V9(2)", "1234E")]
    [DataRow(123.45, "S9(3)V9(5)", "1234500{")]
    [DataRow(123.45, "S9(2)V9(2)",  "234E")]
    [DataRow( 12.3 , "S9(1)V9(3)",  "230{")]
    public void Encode_Double_With_Sign_Default(object value, string picString, string expected)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(value);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }

    [DataTestMethod]
    [DataRow(-123.45, "S9(3)V9(2)", "1234N")]
    [DataRow(-123.4 , "S9(3)V9"   , "123M")]
    [DataRow(-123   , "S9(3)"     , "12L")]
    [DataRow(-123.45, "S9(3)V9(5)", "1234500}")]
    [DataRow(-123.45, "S9(2)V9(2)",  "234N")]
    [DataRow( -12.3 , "S9(1)V9(3)",  "230}")]
    public void Encode_Negative_Double_With_Sign_Default(object value, string picString, string expected)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = PicClauseCodec.ForMeta(pic).Encode(value);

        string result = cp950.GetString(buffer);

        Assert.AreEqual(expected, result);
    }

    [DataTestMethod]
    [DataRow( 12.3 , "S9(3)V9"   , "0123")]
    [DataRow(-12.3 , "S9(3)V9"   , "012L")]
    public void Encode_WithDataStorageOption_ACUCOBOL(object value, string picString, string expected)
    {
        var pic = PicMeta.Parse(picString);
        byte[] buffer = PicClauseCodec.ForMeta(pic).WithDataStorageOption(DataStorageOptions.CA).Encode(value);

        string result = cp950.GetString(buffer);
        
        Assert.AreEqual(expected, result);
    }

    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod]
    public void Encode_With_String_Value_Cause_Exception()
    {
        var pic = PicMeta.Parse("S9(5)V9");

        Assert.ThrowsException<NotSupportedException>(() => PicClauseCodec.ForMeta(pic).Encode("中文字"));
    }
}
