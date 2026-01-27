using CommandLine;

namespace Copycat;

class Program
{
    public sealed class Options
    {
        [Option('f', "file", Required = true, HelpText = "Input copybook file.")]
        public FileInfo? File { get; set; }

        [Option('v', "verbose", HelpText = "Enable verbose output.")]
        public bool Verbose { get; set; }
    }

    static void Main(string[] args)
    {      
        Parser.Default.ParseArguments<Options>(args)
            .WithParsed(RunOptions)
            .WithNotParsed(HandleParseError);
    }

    // ----------------------------------------

    private static void RunOptions(Options opts)
    {
        ArgumentNullException.ThrowIfNull(opts.File);

        if (!opts.File.Exists)
        {
            Console.Error.WriteLine($"File not found: {opts.File.FullName}");
            Environment.Exit(1);
        }

        ReadFile(opts.File, opts.Verbose);
    }

    private static void HandleParseError(IEnumerable<Error> errors)
    {
        if (errors.Any(e => e is HelpRequestedError or VersionRequestedError))
        {
            Environment.Exit(0); // 使用者只是要看 help
        }

        Environment.Exit(1);
    }

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

