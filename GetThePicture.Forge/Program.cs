using CommandLine;

namespace GetThePicture.Forge;

class Program
{
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

        // TODO: ...

        return 0;
    }

    private static int HandleParseError(IEnumerable<Error> errors)
    {
        return errors.Any(e => e is HelpRequestedError or VersionRequestedError) ? 0 : 1;
    }
}
