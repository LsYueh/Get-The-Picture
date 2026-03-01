using GetThePicture.Cobol.Base;
using GetThePicture.Copybook.Compiler.Layout.Base;

namespace GetThePicture.Copybook.Compiler.Layout.Item;

public sealed class Renames66Item(
    Area_t area,
    string name, string from, string? through, string? comment = null
) : DataItem(area, 66, name, null, comment)
{
    // ----------------------------
    // RENAMES
    // ----------------------------

    public string FromName { get; init; } = from;
    public ElementaryDataItem From { get; private set; } = null!;

    public string? ThruName { get; init; } = through;
    public ElementaryDataItem? Thru { get; private set; } = null;

    /// <summary>
    /// 解析 66 層級 RENAMES，對應 From ~ Through 範圍
    /// </summary>
    /// <param name="layout"></param>
    /// <exception cref="InvalidOperationException"></exception>
    public void SetAffectedItems(IReadOnlyList<IDataItem> flatten)
    {
        int start = -1;
        for (int i = 0; i < flatten.Count; i++)
        {
            if (flatten[i].Name == FromName)
            {
                start = i;
                From = ValidateTarget(flatten[i], "from");
                break;
            }
        }

        if (start < 0)
            throw new InvalidOperationException(
                $"RENAMES from '{FromName}' not found.");

        int end = start;
        if (!string.IsNullOrEmpty(ThruName))
        {
            for (int i = start; i < flatten.Count; i++)
            {
                if (flatten[i].Name == ThruName)
                {
                    end = i;
                    Thru = ValidateTarget(flatten[i], "thru");
                    break;
                }
            }

            if (end < 0)
                throw new InvalidOperationException(
                    $"RENAMES thru '{ThruName}' not found.");
        }

        if (end < start)
            throw new InvalidOperationException(
                $"RENAMES range invalid: {FromName} thru {ThruName}");
    }

    private static ElementaryDataItem ValidateTarget(IDataItem item, string role)
    {
        if (item.Level is 01 or 77 or 88 or 66)
            throw new InvalidOperationException(
                $"RENAMES {role} '{item.Name}' cannot reference level {item.Level} items.");
        
        if (item is not ElementaryDataItem e)
            throw new InvalidOperationException(
                $"RENAMES {role} '{item.Name}' must reference an Elementary Data Item.");

        if (e.Occurs is not null && e.Occurs > 0)
            throw new InvalidOperationException(
                $"RENAMES {role} '{item.Name}' cannot reference an OCCURS item.");

        return e;
    }

    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter w, int indent = 0)
    {
        w.Write($"{Indent(indent)}66 {Name} >> Renames {FromName}");

        if (!string.IsNullOrEmpty(ThruName))
        {
            w.Write(" through ");
            w.Write(ThruName);
        }

        w.WriteLine();

        From.Dump(w, indent + 1);
        Thru?.Dump(w, indent + 1);
    }

    private string FormatComment() => (Comment != null) ? $" [{Comment}]" : "";
}