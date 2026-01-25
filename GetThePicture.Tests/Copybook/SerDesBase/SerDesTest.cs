using System.Text;

using GetThePicture.Codec.Utils;
using GetThePicture.Copybook;
using GetThePicture.Copybook.SerDesBase;

namespace GetThePicture.Tests.Copybook.SerDesBase;

[TestClass]
public class SerDesTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    public void Deserialize_T30_OTC_Test()
    {
        var schema = Reader.FromStreamReader(new StreamReader(@"TestData/t30-otc.cpy", cp950));
    
        var serDes = new SerDes(schema);

        using var reader = new StreamReader(@"TestData/t30-otc-lite.dat", cp950);

        string? line;
        while ((line = reader.ReadLine()) != null)
        {
            var bytes = cp950.GetBytes(line);
            var record = serDes.Deserialize(bytes);

            Assert.AreEqual(19, record.Fields.Count);
            record.Fields.TryGetValue("MARK-W-DETAILS", out object? value);
            
            if (value is RecordValue subRecord)
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