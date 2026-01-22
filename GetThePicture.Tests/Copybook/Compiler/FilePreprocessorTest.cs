using System.Text;

using GetThePicture.Codec.Utils;
using GetThePicture.Copybook.Compiler;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]
public class FilePreprocessorTest
{
    private const string filePath = @"TestData/TEST-SAMPLE.CPY";

    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void ToDocumentLine()
    {        
        using var reader = new StreamReader(filePath, cp950);

        var docLines = FilePreprocessor.Process(reader);

        Assert.IsTrue(docLines.Count > 0, "文件應該有內容");
    }
}
