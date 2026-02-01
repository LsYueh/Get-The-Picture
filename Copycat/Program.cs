using CommandLine;

using Copycat.Commands;

namespace Copycat;

class Program
{
    public sealed class Options
    {
        [Option('s', "schema", Required = true, HelpText = "Input copybook schema.")]
        public FileInfo? Schema { get; set; }

        [Option('o', "output", Required = false, HelpText = "Output C# Value Objects file.")]
        public string? Output { get; set; }

        [Option('f', "file", Required = false, HelpText = "Input COBOL data file.")]
        public FileInfo? Data { get; set; }

        [Option("with-renames-66", HelpText = "Generate properties for COBOL 66-level conditions.")]
        public bool EmitRenames66 { get; set; }

        [Option("with-condition-88", HelpText = "Generate properties for COBOL 88-level conditions.")]
        public bool EmitCondition88 { get; set; }

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

        CodeGenOptions options = new()
        {
            EmitCondition66 = opts.EmitRenames66,
            EmitCondition88 = opts.EmitCondition88,
        };

        SchemaCmd schemaCmd = new(options);

        var schema = SchemaCmd.ReadSchema(opts.Schema, opts.Verbose);

        string fileName = opts.Output ?? "Out.cs";
        schemaCmd.CodeGen(schema, fileName);

        Console.WriteLine($"New sealed class generated: \"{Path.GetFullPath(fileName)}\"");

        return 0;
    }

    private static int HandleParseError(IEnumerable<Error> errors)
    {
        return errors.Any(e => e is HelpRequestedError or VersionRequestedError) ? 0 : 1;
    }
}

