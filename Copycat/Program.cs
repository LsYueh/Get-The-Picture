using CommandLine;

using Copycat.Commands;

namespace Copycat;

class Program
{
    public sealed class Options
    {
        [Option('s', "schema", Required = true, HelpText = "Input copybook schema.")]
        public FileInfo? Schema { get; set; }

        [Option('f', "file", Required = false, HelpText = "Input COBOL data file.")]
        public FileInfo? Data { get; set; }

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
        if (!opts.Schema!.Exists)
        {
            Console.Error.WriteLine($"File not found: {opts.Schema.FullName}");
            return 1;
        }

        var schema = SchemaCmd.ReadSchema(opts.Schema, opts.Verbose);

        return 0;
    }

    private static int HandleParseError(IEnumerable<Error> errors)
    {
        return errors.Any(e => e is HelpRequestedError or VersionRequestedError) ? 0 : 1;
    }
}

