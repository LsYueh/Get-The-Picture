using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Codec.Semantic;

public static class Rules
{
    // TODO: 看看 Date/Time/Timestamp 要不要支援 COMP-3

    private static readonly Dictionary<PicSemantic, Constraint> _rules = new() {
        {
            PicSemantic.GregorianDate, new Constraint
            {
                AllowedUsage = [PicUsage.Display],
                RequiredLength = 8,
                RequiredDecimalDigits = 0,
                MustBeSigned = false,
            }
        },
        {
            PicSemantic.MinguoDate, new Constraint
            {
                AllowedUsage = [PicUsage.Display],
                RequiredLength = 7,
                RequiredDecimalDigits = 0,
                MustBeSigned = false,
            }
        },
        {
            PicSemantic.Time6, new Constraint
            {
                AllowedClasses = [PicBaseClass.Numeric, PicBaseClass.Alphanumeric],
                AllowedUsage = [PicUsage.Display],
                RequiredLength = 6,
                RequiredDecimalDigits = 0,
                MustBeSigned = false,
            }
        },
        {
            PicSemantic.Time9, new Constraint
            {
                AllowedClasses = [PicBaseClass.Numeric, PicBaseClass.Alphanumeric],
                AllowedUsage = [PicUsage.Display],
                RequiredLength = 9,
                RequiredDecimalDigits = 0,
                MustBeSigned = false,
            }
        },
        {
            PicSemantic.Timestamp14, new Constraint
            {
                AllowedClasses = [PicBaseClass.Numeric, PicBaseClass.Alphanumeric],
                AllowedUsage = [PicUsage.Display],
                RequiredLength = 14,
                RequiredDecimalDigits = 0,
                MustBeSigned = false,
            }
        },
        {
            PicSemantic.Boolean, new Constraint
            {
                AllowedUsage = [PicUsage.Display],
                RequiredLength = 1,
                RequiredDecimalDigits = 0,
                MustBeSigned = false,
            }
        }
    };

    public static Constraint GetConstraint(PicSemantic semantic)
    {
        if (!_rules.TryGetValue(semantic, out var rule))
            throw new InvalidOperationException($"No rule defined for {semantic}");

        return rule;
    }
}
