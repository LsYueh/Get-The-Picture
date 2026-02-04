using GetThePicture.Copybook.Base;
using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Layout.Base;

namespace GetThePicture.Copybook.Compiler;

public sealed class CbCompiler
{
    public static CbLayout FromStreamReader(StreamReader streamReader)
    {
        ArgumentNullException.ThrowIfNull(streamReader);

        var lines = CobolLine.FromStreamReader(streamReader);
        Lexer lexer = new(lines);

        var tokens = lexer.Tokenize();
        Parser parser = new(tokens);

        var obj = parser.Analyze();
        if (obj is not CbLayout layout)
            throw new Exception("Copybook root must be a Document.");

        layout.CalculateStorage();

        FinalizeLayout(layout);

        return layout;
    }

    /// <summary>
    /// 完成 Copybook 的語意關聯，供後續 C# 生成或序列化使用
    /// </summary>
    /// <param name="layout"></param>
    private static void FinalizeLayout(CbLayout layout)
    {
        SetRedefinesTargets(layout.Children);

        // Level 66
        SetRenames66(layout);
    }

    /// <summary>
    /// 解析 66 層級 RENAMES，對應 From ~ Through 範圍
    /// </summary>
    /// <param name="root"></param>
    /// <param name="seqList"></param>
    /// <exception cref="InvalidOperationException"></exception>
    private static void SetRenames66(IDataItem root)
    {
        var seqList = BuildSequentialElementaryDataItemList(root);
        
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
    /// 展開所有 ElementaryDataItem 為線性列表（提供 66 RENAMES 解析用）
    /// </summary>
    /// <param name="root"></param>
    /// <returns></returns>
    private static List<ElementaryDataItem> BuildSequentialElementaryDataItemList(IDataItem root)
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

        Walk(root);

        return list;
    }

    /// <summary>
    /// 解析 REDEFINES，找到 RedefinesItem 內的 Target 所對應的 ElementaryDataItem
    /// </summary>
    /// <param name="items"></param>
    /// <exception cref="CompileException"></exception>
    public static void SetRedefinesTargets(IEnumerable<IDataItem> items)
    {
        foreach (var item in items)
        {
            if (item is RedefinesItem r)
            {
                // 找到同層的 target
                IDataItem? target = items.OfType<IDataItem>().FirstOrDefault(e => e.Name == r.TargetName);

                if (target is null)
                    throw new CompileException($"Cannot resolve REDEFINES target '{r.TargetName}' for '{r.Name}' '{r.Name}'.");

                r.SetTarget(target);
            }

            // 如果是 group，有 children，也要遞迴
            if (item is GroupItem g && g.Children.Any())
                SetRedefinesTargets(g.Children);
        }
    }
}