using CommandLine;

using Copycat.Commands;

namespace Copycat;

class Program
{
    public sealed class Options
    {
        [Option('s', "layout", Required = true, HelpText = "Input copybook layout.")]
        public FileInfo? Layout { get; set; }

        [Option('o', "output", Required = false, HelpText = "Output C# Value Objects file.")]
        public string? Output { get; set; }

        [Option('f', "file", Required = false, HelpText = "Input COBOL data file.")]
        public FileInfo? Data { get; set; }

        [Option("with-redefines", HelpText = "Generate properties for COBOL REDEFINES.")]
        public bool EmitRedefines { get; set; }

        [Option("with-renames-66", HelpText = "Generate properties for COBOL 66-level RENAMES.")]
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
        if (!opts.Layout!.Exists)
        {
            Console.Error.WriteLine($"File not found: {opts.Layout.FullName}");
            return 1;
        }

        CodeGenOptions options = new()
        {
            EmitRedefines   = opts.EmitRedefines,
            EmitCondition66 = opts.EmitRenames66,
            EmitCondition88 = opts.EmitCondition88,
        };

        LayoutCmd layoutCmd = new(options);

        var layout = LayoutCmd.ReadLayout(opts.Layout, opts.Verbose);

        string fileName = opts.Output ?? "Out.cs";
        layoutCmd.CodeGen(layout, fileName);

        Console.WriteLine($"New sealed class generated: \"{Path.GetFullPath(fileName)}\"");

        return 0;
    }

    private static int HandleParseError(IEnumerable<Error> errors)
    {
        return errors.Any(e => e is HelpRequestedError or VersionRequestedError) ? 0 : 1;
    }
}

