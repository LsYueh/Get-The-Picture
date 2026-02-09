using System.Text;

using GetThePicture.Copybook.Provider;
using GetThePicture.Obsolete.SerDes;
using GetThePicture.Obsolete.SerDes.Record;
using GetThePicture.Picture.Clause.Utils;

// using GetThePicture.Test.Utils;

namespace GetThePicture.Tests.Obsolete.SerDes;

[TestClass]
public class CbSerDesTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    [TestCategory("Demo")]
    public void Deserialize_Serialize_T30_OTC_Test()
    {
        var provider = new DataProvider(new StreamReader(@"TestData/twse/t30-otc.cpy", cp950));
        var serDes = new CbSerDes(provider);

        Assert.AreEqual(100, provider.GetLayout().StorageOccupied);

        // provider.GetStorage().Dump(Console.Out);

        using var reader = new StreamReader(@"TestData/twse/t30-otc-lite.dat", cp950);

        // 用 StreamReader() 處理帶有換行符號的資料

        string? line;
        while ((line = reader.ReadLine()) != null)
        {
            var expected = cp950.GetBytes(line);

            Assert.AreEqual(provider.GetLayout().StorageOccupied, expected.Length);

            var record = serDes.Deserialize(expected);

            // Console.WriteLine("==== Record ====");
            // record.Print();
            // Console.WriteLine("================\n");

            Assert.AreEqual(19, record.Fields.Count);
            record.Fields.TryGetValue("MARK-W-DETAILS", out object? value);
            
            if (value is CbRecord subRecord)
                Assert.AreEqual(4, subRecord.Fields.Count);
            else
            {
                Assert.Fail();
            }

            var serialized = serDes.Serialize(record);

            CollectionAssert.AreEqual(expected, serialized);
        }
    }

    [TestMethod]
    [TestCategory("Demo")]
    public void SerDes_Nested_Occurs_Record_Test()
    {
        var provider = new DataProvider(new StreamReader(@"TestData/nested-occurs-record.cpy", cp950));
        var serDes = new CbSerDes(provider);

        using var fs = new FileStream(@"TestData/nested-occurs-record.dat", FileMode.Open, FileAccess.Read);
        using var reader = new BinaryReader(fs, cp950);

        // 用 FileStream() 搭配 BinaryReader() 處理 COBOL 匯出無換行且連續的資料

        int recordLength = provider.GetLayout().StorageOccupied;
        while (fs.Position < fs.Length)
        {
            byte[] buffer = reader.ReadBytes(recordLength);
            if (buffer.Length < recordLength)
                break; // 殘筆
                
            var record = serDes.Deserialize(buffer);

            // Console.WriteLine("==== Record ====");
            // record.Print();
            // Console.WriteLine("================\n");

            var serialized = serDes.Serialize(record);

            // ByteBuffer.Print(buffer);
            // ByteBuffer.Print(serialized);

            CollectionAssert.AreEqual(buffer, serialized);
        }
    }

    [TestMethod]
    [TestCategory("Demo")]
    public void SerDes_Occurs_With_Levle_88_Test()
    {
        var provider = new DataProvider(new StreamReader(@"TestData/occurs-with-levle-88.cpy", cp950));
        var serDes = new CbSerDes(provider);

        using var fs = new FileStream(@"TestData/occurs-with-levle-88.dat", FileMode.Open, FileAccess.Read);
        using var reader = new BinaryReader(fs, cp950);

        // 用 FileStream() 搭配 BinaryReader() 處理 COBOL 匯出無換行且連續的資料

        int recordLength = provider.GetLayout().StorageOccupied;
        while (fs.Position < fs.Length)
        {
            byte[] buffer = reader.ReadBytes(recordLength);
            if (buffer.Length < recordLength)
                break; // 殘筆

            var record = serDes.Deserialize(buffer);

            // Console.WriteLine("==== Record ====");
            // record.Print();
            // Console.WriteLine("================\n");

            var serialized = serDes.Serialize(record);

            // ByteBuffer.Print(buffer);
            // ByteBuffer.Print(serialized);

            CollectionAssert.AreEqual(buffer, serialized);
        }
    }
}