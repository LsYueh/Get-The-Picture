using System.Text;

using CommandLine;
using Microsoft.Extensions.Configuration;

using GetThePicture.Copybook.Provider;
using GetThePicture.Picture.Clause.Utils;

using GetThePicture.Forge.Commands.Wrapper;
using GetThePicture.Forge.Core;
using GetThePicture.Forge.Core.Config;

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
        if (opts.Copybook is null || !opts.Copybook.Exists)
        {
            Console.Error.WriteLine($"Copybook not found: {opts.Copybook?.FullName}");
            return 1;
        }

        try
        {
            var config = new ForgeConfig(BuildConfiguration(opts));

            WrapperCommand cmd = new (config);
            
            using var reader = new StreamReader(opts.Copybook.FullName, CP950);
            var provider = new DataProvider(reader);

            if (opts.Verbose) {
                DumpDataProvider(provider);
            }

            string fileName = NamingHelper.ToPascalCase(Path.GetFileNameWithoutExtension(opts.Copybook.FullName));
            
            cmd.ForgeCode(provider, fileName);

            Console.WriteLine($"New wrapper class generated: \"{Path.GetFullPath($"{fileName}.cs")}\"");

            return 0;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine("Forge execution failed.");
            Console.Error.WriteLine(ex.Message);

#if DEBUG
            Console.Error.WriteLine(ex.StackTrace);
#endif
            return 1;
        }
    }

    private static IConfiguration BuildConfiguration(Options opts)
    {
        var builder = new ConfigurationBuilder();

        // global config
        builder.AddJsonFile("forge.json", optional: true);

        // per copybook config
        var localConfig = Path.ChangeExtension(opts.Copybook!.FullName, ".forge.json");
        builder.AddJsonFile(localConfig, optional: true);

        if (File.Exists(localConfig))
        {
            Console.WriteLine($"⚠ Local configuration detected: {localConfig}");
        }

        return builder.Build();
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

    private static int HandleParseError(IEnumerable<Error> errors)
    {
        return errors.Any(e => e is HelpRequestedError or VersionRequestedError) ? 0 : 1;
    }
}
