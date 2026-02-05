# PICTURE Clause Codec

PICTURE Clause Codec 負責在 **CLR 型別** 與 **COBOL Elementary Data Item 的實體儲存格式** 之間進行雙向轉換。  
專注於 **PICTURE Clause** 語法的編碼與解碼行為，提供一個對熟悉 COBOL 資料表示方式的使用者而言，直觀且可預期的轉換機制。

<br>

## Encoder

```
CLR value
↓
COB Meta (PICTURE / USAGE / SIGN / SCALE)
↓
COBOL Elementary Item (buffer)
```

<br>

在 Encoder 階段，`COB Meta` 主要用於進行資訊降維：  
將 CLR 型別中所攜帶的高階型別與語意資訊，映射為傳統 COBOL 環境中僅能以**文字**與**數字**表示的資料形式，並保留讓 PICTURE Clause 可進行實體編碼所需的最小必要資訊。

<br>

## Decoder

```
COBOL Elementary Item (buffer)
↓
CLR value
```

<br>

Decoder 則負責反向操作，從實體 buffer 中擷取對應長度的資料，依 PICTURE Clause 的語意解析並還原成 CLR 可用的值。

<br><br>
