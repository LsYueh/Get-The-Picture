using System.Text;

using GetThePicture.Cobol.Base;
using GetThePicture.Copybook.Compiler.Base;

namespace GetThePicture.Tests.Copybook.Compiler.Base;

[TestClass]

public class ParserForRedefinesTest
{
    private static readonly Lexer lexer = new();

    [DataTestMethod]
    [DataRow(
        "05 A PIC X." + 
        "05 B REDEFINES A.",
        "COPYBOOK-LAYOUT", "05 B REDEFINES A.")]
    [DataRow(
        "05 FIELD-DATA PIC X(126)." +
        "05 COMT-DATA REDEFINES FIELD-DATA.",
        "COPYBOOK-LAYOUT", "05 COMT-DATA REDEFINES FIELD-DATA.")]
    public void Test_Set(string line, string expected_01, string expected_02)
    {        
        var tokens = lexer.Tokenize(line, 1, Area_t.Free).ToList();

        Parser parser = new(tokens);

        var layout = parser.Analyze();
        layout.Seal();
        
        Assert.IsNotNull(layout);

        var sb = new StringBuilder();
        using var writer = new StringWriter(sb);

        layout.Dump(writer);

        string result = sb.ToString();
        
        StringAssert.Contains(result, expected_01);
        StringAssert.Contains(result, expected_02);
    }

    [DataTestMethod]
    [DataRow(
        "05 B REDEFINES A.",
        "05 A PIC X." + 
        "COPYBOOK-LAYOUT", "05 B REDEFINES A.")]
    [DataRow(
        "05 COMT-DATA REDEFINES FIELD-DATA.",
        "05 FIELD-DATA PIC X(126)." +
        "COPYBOOK-LAYOUT", "05 COMT-DATA REDEFINES FIELD-DATA.")]
    [ExpectedException(typeof(CompileException))]
    public void Test_Throw_CompileException(string line, string expected_01, string expected_02)
    {
        var tokens = lexer.Tokenize(line, 1, Area_t.Free).ToList();

        Parser parser = new(tokens);

        var layout = parser.Analyze();
        layout.Seal();
    }
}