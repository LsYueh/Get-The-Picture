# Copycat

將 `Copybook` 中的 PICTURE / 資料欄位結構，轉換成可在 C# 使用的強型別資料模型。

<br>

# 使用方式

使用 `--layout` 指定要轉換的 Copybook  
```bash
copycat --layout nested-occurs-record.cpy
```

輸出結果:  
```bash
New sealed class generated: "D:\Projects\get-the-picture\GetThePicture.Tests\TestData\Out.cs"
```

<br>

產出的資料模型 `Out.cs` 內容:  
```csharp
namespace GeneratedCopybook;

/// <summary>
/// Record Size : 114
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

<br>

# 參數說明

## `-v`, `--verbose`
額外顯示 Copybook 解析後的資料綱要內容

```bash
copycat --layout nested-occurs-record.cpy --verbose
```

<br>

輸出結果:
```bash
==== LAYOUT ====
COPYBOOK-LAYOUT
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

<br><br>

## `--with-redefines`
⚠️ 由於 C# 與 COBOL 的語言差異，`REDEFINES` 目前只會在程式碼中產生註解說明，不會生成實際屬性或欄位。

```bash
copycat --layout copybook-with-redefines.cpy --with-redefines
```

<br>
註解內容會反應被重新定義的目標  

```csharp
    ...

    /// <summary>
    /// REDEFINES 'B' overlays target 'A'
    /// Target type: ElementaryDataItem
    /// Target PIC: [X(6)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=6, Dec=0, Len=6, Usage='Display'
    /// This property overlays the target's storage.
    /// </summary>
    // TODO: generate property mapping to target after resolving Target.

    ...

```

<br><br>

## `--with-renames-66`
資料模型會根據 `66層級` 的內容產對應的宣告

```bash
copycat --layout EMPREC.CPY --with-renames-66
```

`EMPREC.CPY`
```cobol
 01  EMP-RECORD.
    05 EMP-ID           PIC X(10).
    05 EMP-NAME         PIC X(30).
    05 EMP-DEPT         PIC X(04).
    05 EMP-SALARY       PIC 9(07)V99.
 66  EMP-KEY RENAMES EMP-ID THRU EMP-DEPT.
```

<br>
加入參數後，會額外生成一個「獨立 class」來表示 66  

```csharp
public sealed class EmpRecord
{
    // (省略)...
}

/// <summary>
/// 66 'EMP-KEY' RENAMES logical grouping. <br />
/// Original range: 'EMP-ID' through 'EMP-DEPT'. <br />
/// This item represents semantic grouping only and does not occupy storage. <br />
/// Affected elementary items: 3.
/// </summary>
public sealed class EmpKey66
{
    public static readonly string[] Fields = [
        "EmpId",
        "EmpName",
        "EmpDept",
    ];
}

```

<br><br>

## `--with-condition-88`
資料模型會根據 `88層級` 的內容產出對應的判斷式

```bash
copycat --layout nested-occurs-record.cpy --with-condition-88
```

`occurs-with-levle-88.cpy`
```cobol
 01 ORDER-TABLE.
    05 ORDER-STATUS PIC X OCCURS 5 TIMES.
        88 ORDER-SHIPPED   VALUE 'S'.
        88 ORDER-PENDING   VALUE 'P'.
        88 ORDER-CANCELLED VALUE 'C'.
```

<br>

一般情況下根據產`occurs-with-levle-88.cpy`出的資料模型
```csharp
/// <summary>
/// Record Size : 5
/// </summary>
public sealed class OrderTable
{
    /// <summary>
    /// X [1] Occurs: 5
    /// </summary>
    public string[] OrderStatus { get; } = new string[5];

}
```

<br>

加入參數 `--with-condition-88` 後的輸出結果:
```csharp
/// <summary>
/// Record Size : 5
/// </summary>
public sealed class OrderTable
{
    /// <summary>
    /// X [1] Occurs: 5
    /// </summary>
    public string[] OrderStatus { get; } = new string[5];
    public bool IsOrderShippedAt(int index)
    {
        if (index >= OrderStatus.Length)
            throw new IndexOutOfRangeException();
        return OrderStatus[index] == "S";
    }
    public bool IsOrderPendingAt(int index)
    {
        if (index >= OrderStatus.Length)
            throw new IndexOutOfRangeException();
        return OrderStatus[index] == "P";
    }
    public bool IsOrderCancelledAt(int index)
    {
        if (index >= OrderStatus.Length)
            throw new IndexOutOfRangeException();
        return OrderStatus[index] == "C";
    }

}
```

<br>

## 範例 1：單值 Level-88
COBOL
```cobol
05 STATUS        PIC X.
   88 OK         VALUE 'Y'.
```

<br>

C#（轉換後）
```csharp
public string Status { get; set; } = null!;

public bool IsOk => Status == "Y";
```

<br>

## 範例 2：OCCURS 陣列 + Level-88
COBOL
```cobol
05 STATUS        OCCURS 5 TIMES PIC X.
   88 OK         VALUE 'Y'.
```

<br>

C#（轉換後）
```csharp
public string[] Status { get; } = new string[5];

public bool IsOkAt(int index)
{
    if (index >= Status.Length)
        throw new IndexOutOfRangeException();
    return Status[index] == "Y";
}
```

<br>

## 範例 3：VALUE 多值 / THRU
COBOL
```cobol
05 GRADE         PIC 9.
   88 PASS       VALUE 6 THRU 9.
   88 FAIL       VALUE 0 THRU 5.
```

<br>

C#（轉換後）
```csharp
public byte Grade { get; set; }

public bool IsPass => Grade >= 6 && Grade <= 9;
public bool IsFail => Grade >= 0 && Grade <= 5;
```

<br>

## 範例 4：OCCURS + THRU + Level-88
COBOL
```cobol
05 SCORES        PIC 9 OCCURS 3 TIMES.
   88 HIGH       VALUE 7 THRU 9.
```

<br>

C#（轉換後）
```csharp
public byte[] Scores { get; } = new byte[3];

public bool IsHighAt(uint index)
{
    if (index >= (uint)Scores.Length)
        throw new IndexOutOfRangeException();
    return Scores[index] >= 7 && Scores[index] <= 9;
}
```

<br><br>
