# 文字 (Alphabetic/Alphanumeric)
| COBOL PIC   | 說明                   |  CLR對應  |  編碼  | 解碼  | 說明 |
| ----------- | ---------------------- | :---------: | :----: |:----: | :--: |
| `X(n)`      | 任意字元，長度 n            | `string`  | ✅ |✅ | -- |
| `A(n)`      | 只允許字母                 | `string`  | ✅ |✅ | -- |
| `AN(n)`     | 字母 + 數字                | `string`  | ❌ |❌ | 請用 `X(n)` |
| `G(n)`      | 雙位元組字元 (DBCS, EBCDIC) | `string`  | ❌ |❌ | 請用 `X(n)` |

## 使用方式: 
```csharp
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;
```

```csharp
var pic = Pic.Parse("X(5)");

// Encode: CLR → COBOL PICTURE
CodecBuilder.ForPic(pic).Encode("AbC"); // >> "AbC  "

// Decode: COBOL PICTURE → CLR
CodecBuilder.ForPic(pic).Decode("ABC  "); // >> "ABC"
```

```csharp
var pic = Pic.Parse("X(7)");

CodecBuilder.ForPic(pic).Decode("中文字 "); // >> "中文字"

CodecBuilder.ForPic(pic).Encode("中文字"); // >> "中文字 "
```

```csharp
var pic = Pic.Parse("X(5)");

// 宣告長度不夠
CodecBuilder.ForPic(pic).Encode("中文字"); // >> "中文?"
```

<br><br>
