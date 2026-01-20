# 時間

| COBOL PIC                   |   用途   | CLR對應 |  編碼  |  解碼  |
| --------------------------- | ------- | :-------: | :----: | :----: |
| `X(6)` / `9(6)` (HHmmss)    | 時間     | `TimeOnly` | ✅ | ✅ |
| `X(9)` / `9(9)` (HHmmssSSS) | 時間     | `TimeOnly` | ✅ | ✅ |

## 使用方式:
```csharp
using GetThePicture.Codec;
using GetThePicture.Codec.Utils;
```

```csharp
var pic = Pic.Parse("9(6)"); // X(6) ok!
pic.Semantic = PicSemantic.Time6; // (HHmmss)

// Encode: CLR → COBOL PICTURE
CodecBuilder.ForPic(pic).Encode(new TimeOnly(23, 59, 59, 0)); // >> "235959"

// Decode: COBOL PICTURE → CLR
CodecBuilder.ForPic(pic).Decode("235959"); // >> TimeOnly(23, 59, 59, 0)
```

```csharp
var pic = Pic.Parse("9(9)"); // X(9) ok!
pic.Semantic = PicSemantic.Time9; // (HHmmssfff)

// Encode: CLR → COBOL PICTURE
CodecBuilder.ForPic(pic).Encode(new TimeOnly(12, 30, 45, 678)); // >> "123045678"

// Decode: COBOL PICTURE → CLR
CodecBuilder.ForPic(pic).Decode("123045678"); // >> TimeOnly(12, 30, 45, 678)
```

<br><br>
