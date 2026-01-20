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

## 使用方式:
```csharp
using GetThePicture.Codec;
using GetThePicture.Codec.Utils;
```

```csharp
var pic = Pic.Parse("9(3)");

// Encode: CLR → COBOL PICTURE
CodecBuilder.ForPic(pic).Encode(123); // >> "123"

// Decode: COBOL PICTURE → CLR
CodecBuilder.ForPic(pic).Decode("123"); // >> 123
```

```csharp
var pic = Pic.Parse("S9(3)V9");

// Encode: CLR → COBOL PICTURE
CodecBuilder.ForPic(pic).Encode( 12.3); // >> "012C"
CodecBuilder.ForPic(pic).Encode(-12.3); // >> "012L"

// Encode: CLR → COBOL PICTURE (ACUCOBOL)
CodecBuilder.ForPic(pic).WithDataStorageOption(DataStorageOptions.CA).Decode(12.3); // >> "0123"

// Decode: COBOL PICTURE → CLR
CodecBuilder.ForPic(pic).Decode("12L"); // >> -12.3
```

<br>

### COMP-3

1. 指定 `Usage`  
    ```csharp
    using GetThePicture.Codec;
    using GetThePicture.Codec.Utils;

    using GetThePicture.Cobol.Picture.TypeBase; // 取得 PicUsage
    ```

    ```csharp
    // PIC 9(5).
    var pic = Pic.Parse("9(5)");

    // Encode: CLR → COBOL
    CodecBuilder.ForPic(pic)
        .WithUsage(PicUsage.PackedDecimal) // 動態指定 USAGE COMP-3
        .WithStrict()
        .Encode(52194); // >> [0x52, 0x19, 0x4F]

    // Decode: COBOL → CLR
    CodecBuilder.ForPic(pic)
        .WithUsage(PicUsage.PackedDecimal) // 動態指定 USAGE COMP-3
        .WithStrict()
        .Decode([0x52, 0x19, 0x4C]); // >> 52194UL
    ```

    or

    ```csharp
    // PIC 9(5)  USAGE  COMP-3.
    var pic = Pic.Parse("9(5)");
    pic.Usage = PicUsage.PackedDecimal; // 預先指定

    // Encode: CLR → COBOL
    CodecBuilder.ForPic(pic)
        .WithStrict()
        .Encode(52194); // >> [0x52, 0x19, 0x4F]

    // Decode: COBOL → CLR
    CodecBuilder.ForPic(pic)
        .WithStrict()
        .Decode([0x52, 0x19, 0x4C]); // >> 52194UL
    ```

<br>

2. PIC 的 Truncate 行為  
    ```csharp
    using GetThePicture.Codec;
    using GetThePicture.Codec.Utils;

    using GetThePicture.Cobol.Picture.TypeBase; // 取得 PicUsage
    ```

    ```csharp
    // PIC 9(3)  USAGE  COMP-3.
    var pic = Pic.Parse("S9(3)");
    pic.Usage = PicUsage.PackedDecimal;

    // Encode: CLR → COBOL
    CodecBuilder.ForPic(pic).Encode(-52194); // >> [0x19, 0x4D]

    // Decode: COBOL → CLR
    CodecBuilder.ForPic(pic).Decode([0x52, 0x19, 0x4D]); // >> -194L (-52194L)
    ```

<br><br>
