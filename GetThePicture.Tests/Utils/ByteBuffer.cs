namespace GetThePicture.Tests.Utils;

internal static class ByteBuffer
{
    public static void Print(byte[] buffer, int bytesPerLine = 16)
    {
        for (int i = 0; i < buffer.Length; i += bytesPerLine)
        {
            Console.Write($"{i:X4}: ");

            // Hex
            for (int j = 0; j < bytesPerLine; j++)
            {
                if (i + j < buffer.Length)
                    Console.Write($"{buffer[i + j]:X2} ");
                else
                    Console.Write("   ");
            }

            Console.Write(" | ");

            // ASCII
            for (int j = 0; j < bytesPerLine; j++)
            {
                if (i + j < buffer.Length)
                {
                    byte b = buffer[i + j];
                    Console.Write(b >= 32 && b < 127 ? (char)b : '.');
                }
            }

            Console.WriteLine();
        }

        Console.WriteLine(Environment.NewLine);
    }
}