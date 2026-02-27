using GetThePicture.Copybook.Compiler.Layout;

namespace GetThePicture.Copybook.Compiler.Utils;

public class Renames66()
{
    /// <summary>
    /// 解析 66 層級 RENAMES，對應 From ~ Through 範圍
    /// </summary>
    /// <param name="layout"></param>
    /// <exception cref="InvalidOperationException"></exception>
    public static void SetFrom(CbLayout layout)
    {
        var flatten = layout.GetFlatten();

        var renames66 = layout.GetRenames66();

        foreach (var rename in renames66)
        {
            int start = -1;
            int end = -1;
            
            for (int i = 0; i < flatten.Count; i++)
            {
                if (flatten[i].Name == rename.From)
                {
                    // TODO: 只能允許 Elementary Data Item

                    start = i;
                    break;
                }
            }

            if (start < 0)
                throw new InvalidOperationException($"RENAMES from '{rename.From}' not found.");

            end = start;

            if (!string.IsNullOrEmpty(rename.Thru))
            {
                for (int i = start; i < flatten.Count; i++)
                {
                    if (flatten[i].Name == rename.Thru)
                    {
                        // TODO: 只能允許 Elementary Data Item
                        
                        end = i;
                        break;
                    }
                }

                if (end < 0)
                    throw new InvalidOperationException($"RENAMES thru '{rename.Thru}' not found.");
            }

            if (end < start)
                throw new InvalidOperationException($"RENAMES range invalid: {rename.From} thru {rename.Thru}");

            rename.SetAffectedItems([.. flatten
                .Skip(start)
                .Take(end - start + 1)
                .Select(e => e.Name)]);
        }
    }
}
