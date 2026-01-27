class Program
{
    static void Main(string[] args)
    {
        // 檢查是否有參數
        if (args.Length == 0)
        {
            Console.WriteLine("Hello World! (沒有傳入任何參數)");
        }
        else
        {
            Console.WriteLine("Hello World!");
            Console.WriteLine("你傳入的參數：");

            // 逐個列印參數
            for (int i = 0; i < args.Length; i++)
            {
                Console.WriteLine($"[{i}] {args[i]}");
            }
        }
    }
}

