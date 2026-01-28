using System.Text;

using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;

namespace Copycat.Commands;

public static class SchemaCmd
{
    private static readonly Encoding CP950 = Core.EncodingResolver.CP950;

    public static CbSchema ReadSchema(FileInfo fSchema, bool verbose = true)
    {
        using var reader = new StreamReader(fSchema.FullName, CP950);

        CbSchema schema = CbCompiler.FromStreamReader(reader);

        if (verbose) {
            Console.WriteLine("==== SCHEMA ====");
            schema.Dump(Console.Out);
            Console.WriteLine("================\n");
        }

        return schema;
    }
    
    // public static void Exec(FileInfo file, FileInfo data, bool verbose = true)
    // {
    //     var srSchema = new StreamReader(file.FullName, CP950);
        
    //     var deserializer = new Core.Deserializer(srSchema);

    //     foreach (string line in File.ReadLines(data.FullName, CP950))
    //     {
    //         var buffer = CP950.GetBytes(line);

    //         CbRecord? record = deserializer.Exec(buffer);

    //         if (record is null)
    //         {
    //             if (verbose) Console.Error.WriteLine($"[WARN] Skip invalid record: {line}");
    //             continue;
    //         }
            
    //         if (verbose) {
    //             Console.WriteLine("==== Record ====");
    //             record.Print();
    //             Console.WriteLine("================\n");
    //         }
    //     }
    // }
}
 