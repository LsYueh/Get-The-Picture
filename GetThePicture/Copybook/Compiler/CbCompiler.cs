using GetThePicture.Copybook.Base;
using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;

namespace GetThePicture.Copybook.Compiler;

public sealed class CbCompiler
{
    public static CbLayout FromStreamReader(StreamReader streamReader)
    {
        ArgumentNullException.ThrowIfNull(streamReader);

        var lines = CobolLine.FromStreamReader(streamReader);
        Lexer lexer = new(lines);

        var tokens = lexer.Tokenize();
        Parser parser = new(tokens);

        var obj = parser.Analyze();
        if (obj is not CbLayout layout)
            throw new Exception("Copybook root must be a Layout.");

        layout.Seal();

        return layout;
    }
}
