using System.Text;
using System.Text.Encodings.Web;
using System.Text.Json;

using GetThePicture.Codec.Utils;
using GetThePicture.Copybook;
using GetThePicture.Copybook.Writer;

namespace GetThePicture.Tests.Copybook.Writer;

[TestClass]
public class JsonWriterTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    [TestCategory("Demo")]
    [Ignore]
    public void Writer_Demo()
    {
        var doc = Reader.FromStreamReader(new StreamReader(@"TestData/t30-tse.cpy", cp950));
        Assert.IsNotNull(doc);

        using var stream = new MemoryStream();
        using var writer = new Utf8JsonWriter(stream, new JsonWriterOptions { Indented = true, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping });

        var jsonWriter = new JsonWriter();
       
        jsonWriter.Write(writer, doc);
        writer.Flush();

        string json = Encoding.UTF8.GetString(stream.ToArray());
        
        Console.WriteLine(json);
    }
}