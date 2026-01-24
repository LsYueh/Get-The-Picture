using GetThePicture.Copybook.Base;
using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook;

public sealed class Reader
{
    public static GroupItem FromStreamReader(StreamReader streamReader)
    {
        ArgumentNullException.ThrowIfNull(streamReader);

        var lines = CobolLine.FromStreamReader(streamReader);
        Lexer lexer = new(lines);

        var tokens = lexer.Tokenize();
        Parser parser = new(tokens);

        var ir = parser.Analyze();
        if (ir is not GroupItem model)
            throw new Exception("Copybook root must be a GroupItem.");

        return model;
    }
}