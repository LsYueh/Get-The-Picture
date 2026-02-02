using GetThePicture.Copybook.Base;
using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.Compiler.Ir.Base;

namespace GetThePicture.Copybook.Compiler;

public sealed class CbCompiler
{
    public static CbSchema FromStreamReader(StreamReader streamReader)
    {
        ArgumentNullException.ThrowIfNull(streamReader);

        var lines = CobolLine.FromStreamReader(streamReader);
        Lexer lexer = new(lines);

        var tokens = lexer.Tokenize();
        Parser parser = new(tokens);

        var ir = parser.Analyze();
        if (ir is not CbSchema schema)
            throw new Exception("Copybook root must be a Document.");

        schema.CalculateStorage();

        // === 展開 sequential elementary list（給 66 用）===
        var seqList = BuildSequentialElementaryDataItemList(schema);

        ResolveRenames66(schema, seqList);

        // TODO: 使用 seqList 讓 RedefinesItem 有 Target

        return schema;
    }

    private static List<ElementaryDataItem> BuildSequentialElementaryDataItemList(CbSchema schema)
    {
        var list = new List<ElementaryDataItem>();

        void Walk(IDataItem item)
        {
            switch (item)
            {
                case ElementaryDataItem e:
                    if (e.IsFiller != true)
                        list.Add(e);
                    break;

                default:
                    if (item.Children != null)
                    {
                        foreach (var child in item.Children)
                            Walk(child);
                    }
                    break;
            }
        }

        Walk(schema);

        return list;
    }

    private static void ResolveRenames66(IDataItem root, List<ElementaryDataItem> seqList)
    {
        void Walk(IDataItem item)
        {
            if (item is Renames66Item r)
            {
                int start = seqList.FindIndex(e => e.Name == r.From);
                if (start < 0)
                    throw new InvalidOperationException($"RENAMES from '{r.From}' not found.");

                int end = start;

                if (!string.IsNullOrEmpty(r.Thru))
                {
                    end = seqList.FindIndex(e => e.Name == r.Thru);
                    if (end < 0)
                        throw new InvalidOperationException($"RENAMES thru '{r.Thru}' not found.");
                }

                if (end < start)
                    throw new InvalidOperationException($"RENAMES range invalid: {r.From} thru {r.Thru}");

                r.SetAffectedItems([.. seqList
                    .Skip(start)
                    .Take(end - start + 1)
                    .Select(e => e.Name)]);
            }

            if (item.Children != null)
            {
                foreach (var c in item.Children)
                    Walk(c);
            }
        }

        Walk(root);
    }
}