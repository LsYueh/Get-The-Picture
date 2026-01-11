# 時間戳記

| COBOL PIC                |   用途   | CLR對應 |  編碼  |  解碼  |
| ------------------------ | ------- | :-------: | :----: | :----: |
| `X(14)` (YYYYMMDDHHmmss) | 時間戳記 | `DateTime` | ✅ | ✅ |

> 其他戳記格式可透過 `X(7)` (yyyMMDD) + `X(6)` (HHmmss) = `X(13)` (yyyMMDDHHmmss) 的方式進行組合

## 使用方式:
```csharp
var pic = Pic.Parse("9(14))"); // X(14) ok!
pic.Semantic = PicSemantic.Timestamp14; // (HHmmss)

// Encode: CLR → COBOL PICTURE
CodecBuilder.ForPic(pic).Encode(new DateTime(2024,  1, 15, 12, 30, 45)); // >> "20240115123045"

// Decode: COBOL PICTURE → CLR
CodecBuilder.ForPic(pic).Decode("19991231235959"); // >> DateTime(1999, 12, 31, 23, 59, 59)
```

<br><br>
