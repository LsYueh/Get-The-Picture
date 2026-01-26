using System.Text;
using System.Text.Encodings.Web;
using System.Text.Json;

using GetThePicture.Cobol.Utils;
using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Obsolete.Writer;

namespace GetThePicture.Tests.Copybook.Obsolete.Writer;

[TestClass]
[Ignore]
public class JsonWriterTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    [TestCategory("Obsolete")]
    [Ignore]
    public void Writer_Demo()
    {
        var schema = CbCompiler.FromStreamReader(new StreamReader(@"TestData/t30-tse.cpy", cp950));
        Assert.IsNotNull(schema);

        using var stream = new MemoryStream();
        using var writer = new Utf8JsonWriter(stream, new JsonWriterOptions { Indented = true, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping });

        var jsonWriter = new JsonWriter();
       
        jsonWriter.Write(writer, schema);
        writer.Flush();

        string json = Encoding.UTF8.GetString(stream.ToArray());
        
        Console.WriteLine(json);
    }
}