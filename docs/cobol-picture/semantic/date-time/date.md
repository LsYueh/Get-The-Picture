# 日期

| COBOL PIC                  |  用途  | CLR對應 |  編碼  |  解碼  | 說明 |
| -------------------------- | ------ | :-----: | :----: | :----: | :--: |
| `X(8)` / `9(8)` (YYYYMMDD) |  日期  | `DateOnly` | ✅ | ✅ | 西元年 |
| `X(7)` / `9(7)` (yyyMMDD)  |  日期  | `DateOnly` | ✅ | ✅ | 民國年 |

## 使用方式:
```csharp
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;
```

```csharp
var pic = Pic.Parse("9(8)"); // X(8) ok!
pic.Semantic = PicSemantic.GregorianDate; // (YYYYMMDD)

// Encode: CLR → COBOL PICTURE
CodecBuilder.ForPic(pic).Encode(new DateOnly(2024, 1, 15)); // >> "20240115"

// Decode: COBOL PICTURE → CLR
CodecBuilder.ForPic(pic).Decode("20240115"); // >> DateOnly(2024, 1, 15)
```

```csharp
var pic = Pic.Parse("9(7)"); // X(7) ok!
pic.Semantic = PicSemantic.MinguoDate; // (YYYMMDD)

// Encode: CLR → COBOL PICTURE
CodecBuilder.ForPic(pic).Encode(new DateOnly(2024, 1, 15)); // >> "1130115"

// Decode: COBOL PICTURE → CLR
CodecBuilder.ForPic(pic).Decode("1130115"); // >> DateOnly(2024, 1, 15)
```
<br><br>
