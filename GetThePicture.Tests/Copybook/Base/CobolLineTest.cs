using System.Text;

using GetThePicture.Codec.Utils;
using GetThePicture.Copybook.Base;

namespace GetThePicture.Tests.Copybook.Base;

[TestClass]
public class CobolLineTest
{
    private const string filePath = @"TestData/sample-cobol-copybook.cpy";

    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void Reader_Test()
    {        
        using var reader = new StreamReader(filePath, cp950);

        var lines = CobolLine.FromStreamReader(reader);

        Assert.AreEqual(4, lines.Count);
        Assert.AreEqual("01 LONG-DESCRIPTION.", lines[0].Line);
        Assert.AreEqual("        'NEEDS TO BE CONTINUED ACROSS MULTIPLE LINES'.", lines[^1].Line);
    }
}
