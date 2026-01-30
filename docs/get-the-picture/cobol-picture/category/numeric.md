# 數字 (Numeric)

整數:
| COBOL PIC                   | 位數 (n) | [SIGNED] CLR對應                                                          | [UNSIGNED] CLR對應                                           |  編碼  |  解碼  |
| :-------------------------- | :-----: | :----------------------------------------------------------------------- | :--------------------------------------------------------- | :----: | :----: |
| `9(1)` \~ `9(2)`<br>`S9(1)` \~ `S9(2)`     | 1–2 位   | `sbyte`<br>範圍 **-128 \~ 127**                                              | `byte`<br>範圍 **0 \~ 255**                         | ✅ | ✅ |
| `9(3)` \~ `9(4)`<br>`S9(3)` \~ `S9(4)`     | 3–4 位   | `short`<br>範圍 **-32,768 \~ 32,767**                                        | `ushort`<br>範圍 **0 \~ 65,535**                    | ✅ | ✅ |
| `9(5)` \~ `9(9)`<br>`S9(5)` \~ `S9(9)`     | 5–9 位   | `int`<br>範圍 **-2,147,483,648 \~ 2,147,483,647**                            | `uint`<br>範圍 **0 \~ 4,294,967,295**               | ✅ | ✅ |
| `9(10)` \~ `9(18)`<br>`S9(10)` \~ `S9(18)` | 10–18 位 | `long`<br>範圍 **-9,223,372,036,854,775,808 \~ 9,223,372,036,854,775,807**   | `ulong`<br>範圍 **0 \~ 18,446,744,073,709,551,615** | ✅ | ✅ |
| `9(19)` \~ `9(28)`<br>`S9(19)` \~ `S9(28)` | 19-28 位 | `decimal (scale = 0)`<br>範圍 **約 ±7.9228x10^28**                           | `decimal (scale = 0)`<br>範圍 **約 ±7.9228x10^28**  | ✅ | ✅ |

> 不支援超過`28`位的整數位數  

<br>

浮點數:
| COBOL PIC        |  位數 (n+m) |  說明                         |       CLR對應         |  編碼  |  解碼  |
| ---------------- |  :-------:  | :--------------------------- | :----------------- | :----: | :----: |
| `9(n)V9(m)`  |   1–28 位   | 無號小數，整數 n 位，小數 m 位 | `decimal`<br>範圍 **±1.0x10^-28 \~ ±7.9228x10^28** | ✅ | ✅ |
| `S9(n)V9(m)` |   1–28 位   | 有號小數，整數 n 位，小數 m 位 | `decimal`<br>範圍 **±1.0x10^-28 \~ ±7.9228x10^28** | ✅ | ✅ |

> 不支援超過`28`位的精度位數組合  

<br>

## 使用方式:
```csharp
using GetThePicture.PictureClause;      // PicClauseCodec
using GetThePicture.PictureClause.Base; // PicMeta
```

```csharp
var pic = PicMeta.Parse("9(3)");

// Encode: CLR → COBOL PICTURE
PicClauseCodec.ForMeta(pic).Encode(123); // >> "123"

// Decode: COBOL PICTURE → CLR
PicClauseCodec.ForMeta(pic).Decode("123"); // >> 123
```

```csharp
var pic = PicMeta.Parse("S9(3)V9");

// Encode: CLR → COBOL PICTURE
PicClauseCodec.ForMeta(pic).Encode( 12.3); // >> "012C"
PicClauseCodec.ForMeta(pic).Encode(-12.3); // >> "012L"

// Encode: CLR → COBOL PICTURE (ACUCOBOL)
PicClauseCodec.ForMeta(pic).WithDataStorageOption(DataStorageOptions.CA).Decode(12.3); // >> "0123"

// Decode: COBOL PICTURE → CLR
PicClauseCodec.ForMeta(pic).Decode("12L"); // >> -12.3
```

<br>

### COMP-3
```csharp
using GetThePicture.PictureClause;      // PicClauseCodec
using GetThePicture.PictureClause.Base; // PicMeta
```

```csharp
using GetThePicture.PictureClause.Base.ClauseItems; // PicUsage
```

<br>

1. 指定 `Usage`  
    ```csharp
    // PIC 9(5).
    var pic = PicMeta.Parse("9(5)");

    // Encode: CLR → COBOL
    PicClauseCodec.ForMeta(pic)
        .Usage(PicUsage.PackedDecimal) // 動態指定 USAGE COMP-3
        .WithStrict()
        .Encode(52194); // >> [0x52, 0x19, 0x4F]

    // Decode: COBOL → CLR
    PicClauseCodec.ForMeta(pic)
        .Usage(PicUsage.PackedDecimal) // 動態指定 USAGE COMP-3
        .WithStrict()
        .Decode([0x52, 0x19, 0x4C]); // >> 52194UL
    ```

    or

    ```csharp
    // PIC 9(5)  USAGE  COMP-3.
    var pic = PicMeta.Parse("9(5)");
    pic.Usage = PicUsage.PackedDecimal; // 預先指定

    // Encode: CLR → COBOL
    PicClauseCodec.ForMeta(pic)
        .WithStrict()
        .Encode(52194); // >> [0x52, 0x19, 0x4F]

    // Decode: COBOL → CLR
    PicClauseCodec.ForMeta(pic)
        .WithStrict()
        .Decode([0x52, 0x19, 0x4C]); // >> 52194UL
    ```

<br>

2. PIC 的 Truncate 行為  
    ```csharp
    // PIC 9(3)  USAGE  COMP-3.
    var pic = PicMeta.Parse("S9(3)");
    pic.Usage = PicUsage.PackedDecimal;

    // Encode: CLR → COBOL
    PicClauseCodec.ForMeta(pic).Encode(-52194); // >> [0x19, 0x4D]

    // Decode: COBOL → CLR
    PicClauseCodec.ForMeta(pic).Decode([0x52, 0x19, 0x4D]); // >> -194L (-52194L)
    ```

<br>

### COMP / COMP-5
```csharp
using GetThePicture.PictureClause;      // PicClauseCodec
using GetThePicture.PictureClause.Base; // PicMeta
```

```csharp
using GetThePicture.PictureClause.Base.ClauseItems; // PicUsage
```

<br>

1. 指定 `Usage`  
    ```csharp
    // PIC 9(04)  USAGE  COMP.
    var pic = PicMeta.Parse("S9(04))");
    pic.Usage = PicUsage.Binary;

    // Encode: CLR → COBOL
    PicClauseCodec.ForMeta(pic).WithStrict().Encode(9999); // >> 0x270F >> (Little Endian) >> [0x0F, 0x27]

    // Decode: COBOL → CLR
    PicClauseCodec.ForMeta(pic).WithStrict().Decode([0x0F, 0x27]); // >> (short) 9999
    ```

    ```csharp
    // PIC 9(3)  USAGE  COMP.
    var pic = PicMeta.Parse("S9(4)");
    pic.Usage = PicUsage.PackedDecimal;

    // Decode: COBOL → CLR
    PicClauseCodec.ForMeta(pic).Decode([0x0F, 0x27]); // >> (ushort) 9999
    ```

<br>

2. PIC 內的端序 `Endianness` 行為處理  
    在使用 PIC 搭配二進位儲存格式（COMP/COMP-5） 時，數值在記憶體中的位元組排列順序（Endianness）會因執行平台而有所不同：  
    - Big-Endian（BE）  
    IBM z/OS 主機（Mainframe）環境傳統上採用 Big-Endian，高位元組（Most Significant Byte）儲存在較低的記憶體位址。  

    - Little-Endian（LE）  
    以 Intel 架構為基礎的平台（例如 x86 的 Linux 或 Windows 個人電腦）通常採用 Little-Endian，低位元組（Least Significant Byte）儲存在較低的記憶體位址。  

    故在進行跨平台資料交換或解析 COBOL `COMP`/`COMP-5` 欄位時，必須明確指定並正確處理端序，否則可能導致數值資料轉換錯誤。

    <br>

    ```csharp
    // PIC 9(04)  USAGE  COMP.
    var pic = PicMeta.Parse("S9(04))");
    pic.Usage = PicUsage.Binary;

    // Encode: CLR → COBOL
    PicClauseCodec.ForMeta(pic)
        .WithStrict()
        .WithReversedBinary()
        .Encode(9999); // >> 0x270F >> (Little Endian) >> (Binary Reverse) >> [0x27, 0x0F]

    // Decode: COBOL → CLR
    PicClauseCodec.ForMeta(pic)
        .WithStrict()
        .WithReversedBinary()
        .Decode([0x27, 0x0F]); // >> (short) 9999
    ```

<br><br>
