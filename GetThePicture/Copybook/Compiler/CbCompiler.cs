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

        ResolveSchema(schema);

        return schema;
    }

    /// <summary>
    /// 完成 Copybook IR 的語意關聯，供後續 C# 生成或序列化使用
    /// </summary>
    /// <param name="schema"></param>
    private static void ResolveSchema(CbSchema schema)
    {
        // ========================================
        // Offset 設定開始
        
        // 要先替所有的 DataItem 計算 StorageOccupied
        schema.CalculateStorage();

        // 替 REDEFINES 指定共用 Offset 的目標
        ResolveRedefinesTargets(schema.Children);

        ResolveOffsets(schema);

        // Offset 設定完成
        // ========================================

        // Level 66
        var seqList = BuildSequentialElementaryDataItemList(schema);
        ResolveRenames66(schema, seqList);
    }

    /// <summary>
    /// 展開所有 ElementaryDataItem 為線性列表（提供 66 RENAMES 解析用）
    /// </summary>
    /// <param name="schema"></param>
    /// <returns></returns>
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

    /// <summary>
    /// 解析 66 層級 RENAMES，對應 From ~ Through 範圍
    /// </summary>
    /// <param name="root"></param>
    /// <param name="seqList"></param>
    /// <exception cref="InvalidOperationException"></exception>
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

    /// <summary>
    /// 解析 REDEFINES，找到 RedefinesItem 內的 Target 所對應的 ElementaryDataItem
    /// </summary>
    /// <param name="items"></param>
    /// <exception cref="CompileException"></exception>
    public static void ResolveRedefinesTargets(IEnumerable<IDataItem> items)
    {
        foreach (var item in items)
        {
            if (item is RedefinesItem r)
            {
                // 找到同層的 target
                IDataItem? target = items.OfType<IDataItem>().FirstOrDefault(e => e.Name == r.TargetName);

                if (target is null)
                    throw new CompileException($"Cannot resolve REDEFINES target '{r.TargetName}' for '{r.Name}' '{r.Name}'.");

                if (target.StorageOccupied != r.StorageOccupied)
                    throw new CompileException($"REDEFINES target '{r.TargetName}' storage size mismatch for '{r.Name}'.");

                r.SetTarget(target);
            }

            // 如果是 group，有 children，也要遞迴
            if (item is GroupItem g && g.Children.Any())
                ResolveRedefinesTargets(g.Children);
        }
    }

    public static int ResolveOffsets(IDataItem currentItem, int baseOffset = 0)
    {
        switch(currentItem)
        {
            case ElementaryDataItem e:
            {
                // 遞迴的最末端

                // TODO: 要全部展開...不然巢狀Group後面的ElementaryDataItem會跑掉

                e.SetOffset(baseOffset);
                return e.StorageOccupied * (e.Occurs ?? 1);
            }
            case RedefinesItem r:
            {
                var targetOffset = r.Target.Offset;
                
                int instanceOffset = targetOffset;

                foreach (var child in r.Children)
                {
                    var offset = ResolveOffsets(child, instanceOffset);
                    instanceOffset += offset;
                }

                r.SetOffset(targetOffset);
                
                // REDEFINES：overlay，不推進 offset
                return 0;
            }
            case GroupItem g:
            {
                int occurs = g.Occurs ?? 1;
                int currentOffset = baseOffset;
                
                for (int i = 0; i < occurs; i++)
                {
                    int instanceOffset = currentOffset;

                    foreach (var child in g.Children)
                    {
                        var offset = ResolveOffsets(child, instanceOffset);
                        instanceOffset += offset;
                    }

                    currentOffset = instanceOffset;
                }

                // 檢查目前 GroupItem 佔用的長度是否等同 Children 的總和
                if (currentOffset - baseOffset != g.StorageOccupied * (g.Occurs ?? 1))
                    throw new InvalidOperationException(
                        $"Group '{g.Name}' storage mismatch: " +
                        $"expected {g.StorageOccupied}, actual {currentOffset - baseOffset}");

                g.SetOffset(currentOffset);

                return g.StorageOccupied * (g.Occurs ?? 1);
            }
            default:
                throw new NotSupportedException($"Unsupported IDataItem type: {currentItem.GetType().Name}");
        }
    }
}