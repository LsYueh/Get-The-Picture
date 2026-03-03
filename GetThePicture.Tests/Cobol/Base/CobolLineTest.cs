using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Cobol.Base;
using GetThePicture.Picture.Clause.Utils;
using GetThePicture.TestData;

namespace GetThePicture.Tests.Cobol.Base;

[TestClass]
public class CobolLineTest
{
    private readonly string filePath = TestFileProvider.GetPath("sample-cobol-copybook.cpy");

    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void Reader_Test()
    {        
        using var reader = new StreamReader(filePath, cp950);

        var lines = CobolLine.FromStreamReader(reader);

        Assert.AreEqual(12, lines.Count);

        Assert.AreEqual("01 CUSTOMER-RECORD.", lines[0].Text);
        Assert.AreEqual(Area_t.A, lines[0].Area);

        Assert.AreEqual("        'NEEDS TO BE CONTINUED ACROSS MULTIPLE LINES'.", lines[^1].Text);
        Assert.AreEqual(Area_t.B, lines[^1].Area);
    }
}
