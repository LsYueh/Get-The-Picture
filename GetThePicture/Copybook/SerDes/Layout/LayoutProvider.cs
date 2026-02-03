using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Layout;

namespace GetThePicture.Copybook.SerDes.Layout;

/// <summary>
/// Provides Copybook Layout as runtime layout for SerDes.
/// </summary>
public sealed class LayoutProvider(StreamReader reader) : ILayoutProvider
{
    private readonly Lazy<CbLayout> _layout = new(() => CbCompiler.FromStreamReader(reader));

    public CbLayout GetLayout() => _layout.Value;
}
