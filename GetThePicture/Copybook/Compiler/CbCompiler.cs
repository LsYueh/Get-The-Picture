using GetThePicture.Copybook.Base;
using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Utils;

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

        Finalize(layout);

        return layout;
    }

    /// <summary>
    /// 完成 Copybook 的語意關聯，供後續 C# 生成或序列化使用
    /// </summary>
    /// <param name="layout"></param>
    private static void Finalize(CbLayout layout)
    {
        Redefines.SetTargets(layout);
        Renames66.SetFrom(layout);
    }
}
