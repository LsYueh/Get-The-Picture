using System.Text;

using GetThePicture.Copybook.Provider;
using GetThePicture.Forge.Core;
using GetThePicture.Picture.Clause.Utils;
using static GetThePicture.Forge.Program;

namespace GetThePicture.Forge.Commands.Wrapper;

public sealed class WrapperContext
{
    private static readonly Encoding CP950 = EncodingFactory.CP950;
    
    public DataProvider Provider { get; private set;} 

    public string FileName { get; private set;}

    public bool WithRenames66 { get; private set; }

    public WrapperContext(Options opts)
    {
        Provider = new DataProvider(opts.Copybook!.FullName, CP950);

        if (opts.Verbose) {
            DumpDataProvider(Provider);
        }

        FileName = NamingHelper.ToPascalCase(Path.GetFileNameWithoutExtension(opts.Copybook.FullName));

        WithRenames66 = opts.WithRenames66;
    }

    private static void DumpDataProvider(DataProvider provider)
    {
        Console.WriteLine();
        Console.WriteLine("==== LAYOUT ====");
        provider.GetLayout().Dump(Console.Out);
        Console.WriteLine("================");
        Console.WriteLine();

        Console.WriteLine("==== Storage ====");
        provider.GetStorage().Dump(Console.Out);
        Console.WriteLine("================");
        Console.WriteLine();
    }
}
