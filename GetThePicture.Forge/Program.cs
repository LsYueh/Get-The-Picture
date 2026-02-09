using System.Text;
using CommandLine;

using GetThePicture.Copybook.Provider;
using GetThePicture.Picture.Clause.Utils;

using GetThePicture.Forge.Commands.Warpper;
using GetThePicture.Forge.Core;


namespace GetThePicture.Forge;

class Program
{
    private static readonly Encoding CP950 = EncodingFactory.CP950;
    
    public sealed class Options
    {
        [Option('c', "copybook", Required = true, HelpText = "Path to the copybook file.")]
        public FileInfo? Copybook { get; set; }

        [Option('v', "verbose", HelpText = "Enable verbose output.")]
        public bool Verbose { get; set; }
    }

    static int Main(string[] args)
    {      
        return Parser.Default.ParseArguments<Options>(args)
            .MapResult(
                RunOptions,
                errs => HandleParseError(errs)
            );
    }

    // ----------------------------------------

    private static int RunOptions(Options opts)
    {
        if (!opts.Copybook!.Exists)
        {
            Console.Error.WriteLine($"Copybook not found: {opts.Copybook.FullName}");
            return 1;
        }

        WarpperCommand cmd = new ();

        var provider = new DataProvider(new StreamReader(opts.Copybook.FullName, CP950));

        if (opts.Verbose) {
            Console.WriteLine("==== LAYOUT ====");
            provider.GetLayout().Dump(Console.Out);
            Console.WriteLine("================");
            Console.WriteLine();

            Console.WriteLine("==== Storage ====");
            provider.GetStorage().Dump(Console.Out);
            Console.WriteLine("================");
            Console.WriteLine();
        }

        string fileName = NamingHelper.ToPascalCase(Path.GetFileNameWithoutExtension(opts.Copybook.FullName));
        
        cmd.ForgeCode(provider, fileName);

        Console.WriteLine($"New warpper class generated: \"{Path.GetFullPath($"{fileName}.cs")}\"");

        return 0;
    }

    private static int HandleParseError(IEnumerable<Error> errors)
    {
        return errors.Any(e => e is HelpRequestedError or VersionRequestedError) ? 0 : 1;
    }
}
