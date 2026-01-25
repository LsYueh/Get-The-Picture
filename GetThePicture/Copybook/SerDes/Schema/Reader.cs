using GetThePicture.Copybook.Base;
using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.SerDes.Schema;

public sealed class Reader
{
    public static Document FromStreamReader(StreamReader streamReader)
    {
        ArgumentNullException.ThrowIfNull(streamReader);

        var lines = CobolLine.FromStreamReader(streamReader);
        Lexer lexer = new(lines);

        var tokens = lexer.Tokenize();
        Parser parser = new(tokens);

        var ir = parser.Analyze();
        if (ir is not Document model)
            throw new Exception("Copybook root must be a Document.");

        model.CalculateStorage();

        return model;
    }
}