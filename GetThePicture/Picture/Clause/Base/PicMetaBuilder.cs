using System.Text.RegularExpressions;

using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Symbols;
using GetThePicture.Picture.Symbols.Base;

namespace GetThePicture.Picture.Clause.Base;

internal static partial class PicMetaBuilder
{
    public static PicMeta Parse(string symbols, PicSemantic semantic = PicSemantic.None, PicUsage Usage = PicUsage.Display)
    {
        if (string.IsNullOrWhiteSpace(symbols))
            throw new ArgumentException("PIC clause is empty.");
        
        symbols = symbols.ToUpperInvariant().Replace(" ", string.Empty);
        
        SymbolMeta meta = SymbolParser.Read(symbols);

        var picMeta = meta.BaseClass switch
        {
            // ─────────────────────────
            // Numeric
            // ─────────────────────────
            PicBaseClass.Numeric => new PicMeta {
                Raw           = symbols,
                BaseClass     = meta.BaseClass,
                Semantic      = semantic,
                Usage         = Usage,
                Signed        = meta.Signed,
                IntegerDigits = meta.IntegerDigits,
                DecimalDigits = meta.DecimalDigits
            },

            // ─────────────────────────
            // Alphanumeric
            // ─────────────────────────
            PicBaseClass.Alphanumeric => new PicMeta {
                Raw           = symbols,
                BaseClass     = meta.BaseClass,
                Semantic      = semantic,
                Usage         = PicUsage.Display,
                Signed        = false,
                IntegerDigits = meta.IntegerDigits,
                DecimalDigits = 0
            },

            // ─────────────────────────
            // Alphabetic
            // ─────────────────────────
            PicBaseClass.Alphabetic => new PicMeta {
                Raw           = symbols,
                BaseClass     = meta.BaseClass,
                Semantic      = semantic,
                Usage         = PicUsage.Display,
                Signed        = false,
                IntegerDigits = meta.IntegerDigits,
                DecimalDigits = 0
            },

            _ => throw new NotSupportedException($"Unsupported PIC clause: {symbols}"),
        };

        return picMeta;
    }
}
