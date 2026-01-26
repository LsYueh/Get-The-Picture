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
    public void Deserialize_T30_OTC_Test()
    {
        var schema = CbCompiler.FromStreamReader(new StreamReader(@"TestData/t30-otc.cpy", cp950));

        Assert.AreEqual(100, schema.StorageOccupied);
    
        var serDes = new CbSerDes(schema);

        using var reader = new StreamReader(@"TestData/t30-otc-lite.dat", cp950);

        string? line;
        while ((line = reader.ReadLine()) != null)
        {
            var bytes = cp950.GetBytes(line);

            Assert.AreEqual(schema.StorageOccupied, bytes.Length);

            var record = serDes.Deserialize(bytes);

            Assert.AreEqual(19, record.Fields.Count);
            record.Fields.TryGetValue("MARK-W-DETAILS", out object? value);
            
            if (value is RecValue subRecord)
                Assert.AreEqual(4, subRecord.Fields.Count);
            else
            {
                Assert.Fail();
            }

            // Console.WriteLine("==== Record ====");
            // RecordValuePrinter.Print(record);
            // Console.WriteLine("================\n");
        }
    }
}