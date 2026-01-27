using CommandLine;

namespace Copycat;

class Program
{
    [Verb("run")]
    public sealed class Options
    {
        [Option('f', "file", Required = true, HelpText = "Input copybook file.")]
        public FileInfo? File { get; set; }

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
        ArgumentNullException.ThrowIfNull(opts.File);

        if (!opts.File.Exists)
        {
            Console.Error.WriteLine($"File not found: {opts.File.FullName}");
            return 1;
        }

        ReadFile(opts.File, opts.Verbose);

        return 0;
    }

    private static int HandleParseError(IEnumerable<Error> errors)
    {
        return errors.Any(e => e is HelpRequestedError or VersionRequestedError) ? 0 : 1;
    }

    // ----------------------------------------

    static void ReadFile(FileInfo? file, bool verbose = true)
    {
        ArgumentNullException.ThrowIfNull(file);

        if (!file.Exists)
            throw new FileNotFoundException($"File not found: {file.FullName}", file.FullName);

        foreach (string line in File.ReadLines(file.FullName))
        {
            if (verbose) Console.WriteLine(line);
        }
    }
}

