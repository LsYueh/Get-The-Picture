# 文字 (Alphabetic/Alphanumeric)
| COBOL PIC   | 說明                   |  CLR對應  |  編碼  | 解碼  | 說明 |
| ----------- | ---------------------- | :---------: | :----: |:----: | :--: |
| `X(n)`      | 任意字元，長度 n            | `string`  | ✅ |✅ | -- |
| `A(n)`      | 只允許字母                 | `string`  | ✅ |✅ | -- |
| `AN(n)`     | 字母 + 數字                | `string`  | ❌ |❌ | 請用 `X(n)` |
| `G(n)`      | 雙位元組字元 (DBCS, EBCDIC) | `string`  | ❌ |❌ | 請用 `X(n)` |

## 使用方式: 
```csharp
using GetThePicture.Picture.Clause;      // PicClauseCodec
using GetThePicture.Picture.Clause.Base; // PicMeta
```

```csharp
var pic = PicMeta.Parse("X(5)");

// Encode: CLR → COBOL PICTURE
PicClauseCodec.ForMeta(pic).Encode("AbC"); // >> "AbC  "

// Decode: COBOL PICTURE → CLR
PicClauseCodec.ForMeta(pic).Decode("ABC  "); // >> "ABC"
```

```csharp
var pic = PicMeta.Parse("X(7)");

PicClauseCodec.ForMeta(pic).Decode("中文字 "); // >> "中文字"

PicClauseCodec.ForMeta(pic).Encode("中文字"); // >> "中文字 "
```

```csharp
var pic = PicMeta.Parse("X(5)");

// 宣告長度不夠
PicClauseCodec.ForMeta(pic).Encode("中文字"); // >> "中文?"
```

<br><br>
