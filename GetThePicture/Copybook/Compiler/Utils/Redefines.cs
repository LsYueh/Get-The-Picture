using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Layout.Base;

namespace GetThePicture.Copybook.Compiler.Utils;

public class Redefines()
{
    /// <summary>
    /// 解析 REDEFINES，找到 RedefinesItem 內的 Target 所對應的 ElementaryDataItem
    /// </summary>
    /// <param name="items"></param>
    /// <exception cref="CompileException"></exception>
    public static void SetTargets(CbLayout layout)
    {
        void Walk(IDataItem item)
        {
            foreach (var child in item.Children)
            {
                if (child is RedefinesItem r)
                {
                    // 同級別限制
                    IDataItem? target = item.Children.FirstOrDefault(e => e.Name == r.TargetName);

                    // TODO: 位置順序限制：重新定義的項目必須緊接在被重新定義項目的描述之後。

                    if (target is null)
                        throw new CompileException($"Cannot resolve REDEFINES target '{r.TargetName}' for '{r.Name}'.");

                    // 不能重新定義 66
                    if (target is Renames66Item)
                        throw new CompileException($"Cannot redefine 66-level item '{r.TargetName}' with '{r.Name}'.");

                    // 不能重新定義 88
                    if (target is Condition88Item)
                        throw new CompileException($"Cannot redefine 88-level item '{r.TargetName}' with '{r.Name}'.");

                    r.SetTarget(target);
                }

                // 如果是 group，有 children，也要遞迴
                if (child is GroupItem g)
                    Walk(g);
            }
        }

        Walk(layout);
    }
}
