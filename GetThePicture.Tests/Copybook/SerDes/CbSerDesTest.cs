using System.Text;

using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.SerDes;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.PictureClause.Utils;

namespace GetThePicture.Tests.Copybook.SerDes;

[TestClass]
public class CbSerDesTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    [TestCategory("Demo")]
    public void Deserialize_Serialize_T30_OTC_Test()
    {
        var layout = CbCompiler.FromStreamReader(new StreamReader(@"TestData/twse/t30-otc.cpy", cp950));
        var serDes = new CbSerDes(layout);

        Assert.AreEqual(100, layout.StorageOccupied);

        using var reader = new StreamReader(@"TestData/twse/t30-otc-lite.dat", cp950);

        // 用 StreamReader() 處理帶有換行符號的資料

        string? line;
        while ((line = reader.ReadLine()) != null)
        {
            var expected = cp950.GetBytes(line);

            Assert.AreEqual(layout.StorageOccupied, expected.Length);

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
        var layout = CbCompiler.FromStreamReader(new StreamReader(@"TestData/nested-occurs-record.cpy", cp950));
        var serDes = new CbSerDes(layout);

        using var fs = new FileStream(@"TestData/nested-occurs-record.dat", FileMode.Open, FileAccess.Read);
        using var reader = new BinaryReader(fs, cp950);

        // 用 FileStream() 搭配 BinaryReader() 處理 COBOL 匯出無換行且連續的資料

        int recordLength = layout.StorageOccupied;
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

            // PrintByteBuffer(buffer);
            // PrintByteBuffer(serialized);

            CollectionAssert.AreEqual(buffer, serialized);
        }
    }

    [TestMethod]
    [TestCategory("Demo")]
    public void SerDes_Occurs_With_Levle_88_Test()
    {
        var layout = CbCompiler.FromStreamReader(new StreamReader(@"TestData/occurs-with-levle-88.cpy", cp950));
        var serDes = new CbSerDes(layout);

        using var fs = new FileStream(@"TestData/occurs-with-levle-88.dat", FileMode.Open, FileAccess.Read);
        using var reader = new BinaryReader(fs, cp950);

        // 用 FileStream() 搭配 BinaryReader() 處理 COBOL 匯出無換行且連續的資料

        int recordLength = layout.StorageOccupied;
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

            // PrintByteBuffer(buffer);
            // PrintByteBuffer(serialized);

            CollectionAssert.AreEqual(buffer, serialized);
        }
    }

    static void PrintByteBuffer(byte[] buffer, int bytesPerLine = 16)
    {
        for (int i = 0; i < buffer.Length; i += bytesPerLine)
        {
            Console.Write($"{i:X4}: ");

            // Hex
            for (int j = 0; j < bytesPerLine; j++)
            {
                if (i + j < buffer.Length)
                    Console.Write($"{buffer[i + j]:X2} ");
                else
                    Console.Write("   ");
            }

            Console.Write(" | ");

            // ASCII
            for (int j = 0; j < bytesPerLine; j++)
            {
                if (i + j < buffer.Length)
                {
                    byte b = buffer[i + j];
                    Console.Write(b >= 32 && b < 127 ? (char)b : '.');
                }
            }

            Console.WriteLine();
        }

        Console.WriteLine(Environment.NewLine);
    }

}