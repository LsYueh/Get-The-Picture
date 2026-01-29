namespace GetThePicture.Copybook.SerDes.Record;

internal static class CbRecordPrinter
{
    public static void Print(CbRecord record, int indent = 0)
    {
        string indentStr = new(' ', indent);

        foreach (var kvp in record.Fields)
        {
            switch (kvp.Value)
            {
                case CbRecord nested:
                    Console.WriteLine($"{indentStr}{kvp.Key}:");
                    Print(nested, indent + 2);
                    break;

                case object[] array:
                    Console.WriteLine($"{indentStr}{kvp.Key}: [");
                    foreach (var item in array)
                    {
                        if (item is CbRecord r)
                            Print(r, indent + 4);
                        else
                            Console.WriteLine(new string(' ', indent + 4) + item);
                    }
                    Console.WriteLine($"{indentStr}]");
                    break;

                default:
                    Console.WriteLine($"{indentStr}{kvp.Key}: {kvp.Value}");
                    break;
            }
        }
    }
}
