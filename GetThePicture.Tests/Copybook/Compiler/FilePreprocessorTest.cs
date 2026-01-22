using System.Text;

using GetThePicture.Codec.Utils;
using GetThePicture.Copybook.Compiler;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]
public class FilePreprocessorTest
{
    private const string filePath = @"TestData/sample-cobol-copybook.cpy";

    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void ToDocumentLine()
    {        
        using var reader = new StreamReader(filePath, cp950);

        var lines = FilePreprocessor.Process(reader);

        Assert.IsTrue(lines.Count == 12);
        Assert.AreEqual("01 CUSTOMER-RECORD.", lines[0].Line);
        Assert.AreEqual("        'NEEDS TO BE CONTINUED ACROSS MULTIPLE LINES'.", lines[^1].Line);
    }
}
