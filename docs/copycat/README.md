# Copycat

將 `Copybook` 中的 PICTURE / 資料欄位結構，轉換成可在 C# 使用的強型別資料模型。

<br>

## 使用方式

使用 `--schema` 指定要轉換的 Copybook  
```bash
copycat --schema nested-occurs-record.cpy
```

輸出結果:  
```bash
New sealed class generated: "D:\Projects\get-the-picture\GetThePicture.Tests\TestData\Out.cs"
```

<br>

有參數 `--verbose` 時，會額外顯示 Copybook 解析後的資料綱要內容：  
```bash
==== SCHEMA ====
COPYBOOK-SCHEMA
  1 ORDER-RECORD
    5 ORDER-ID >> PIC: [X(10)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=10, Dec=0, Len=10, Usage='Display'
    5 CUSTOMER-NAME >> PIC: [X(20)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=20, Dec=0, Len=20, Usage='Display'
    5 ORDER-LINES OCCURS 3
      10 PRODUCT-CODE >> PIC: [X(8)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=8, Dec=0, Len=8, Usage='Display'
      10 QUANTITY >> PIC: [9(3)] Class='Numeric' (Semantic='None'), Signed=False, Int=3, Dec=0, Len=3, Usage='Display'
      10 LINE-AMOUNTS OCCURS 2
        15 AMOUNT >> PIC: [9(5)V99] Class='Numeric' (Semantic='None'), Signed=False, Int=5, Dec=2, Len=7, Usage='Display'
    5 TOTAL-AMOUNT >> PIC: [9(7)V99] Class='Numeric' (Semantic='None'), Signed=False, Int=7, Dec=2, Len=9, Usage='Display'
================

New file generated: "D:\Projects\get-the-picture\GetThePicture.Tests\TestData\Out.cs"
```

<br>

產出的資料模型 `Out.cs` 內容:  
```csharp
namespace GeneratedCopybook;

/// <summary>
/// Record Size : 114 <br />
/// </summary>
public sealed class OrderRecord
{
    /// <summary>
    /// X(10) [10]
    /// </summary>
    public string OrderId { get; set; } = null!;

    /// <summary>
    /// X(20) [20]
    /// </summary>
    public string CustomerName { get; set; } = null!;

    public sealed class OrderLines_t
    {
        /// <summary>
        /// X(8) [8]
        /// </summary>
        public string ProductCode { get; set; } = null!;

        /// <summary>
        /// 9(3) [3]
        /// </summary>
        public ushort Quantity { get; set; }

        public sealed class LineAmounts_t
        {
            /// <summary>
            /// 9(5)V99 [7]
            /// </summary>
            public decimal Amount { get; set; }

        }

        /// <summary>
        /// Occurs: 2
        /// </summary>
        public LineAmounts_t[] LineAmounts { get; } = [
            new LineAmounts_t(),
            new LineAmounts_t()
        ];

    }

    /// <summary>
    /// Occurs: 3
    /// </summary>
    public OrderLines_t[] OrderLines { get; } = [
        new OrderLines_t(),
        new OrderLines_t(),
        new OrderLines_t()
    ];

    /// <summary>
    /// 9(7)V99 [9]
    /// </summary>
    public decimal TotalAmount { get; set; }

}

```