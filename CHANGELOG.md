# Changelog

## [26.11.6] – (unreleased)

### Bugfix
- 修正 `COMP-4` 要維持 **Big Endian** 的方式來處理資料。

<br><br>

## [26.11.5] – 2026-02-15

### Changed
- `COMP-3` / `COMP-6` 現在會根據 PICTURE 子句的宣告格式，映射對應的 CLR 資料型態。
    - 原本 `COMP-3`: **整數** > `ulong` / `long`， **浮點數** > `decimal`。
    - 原本 `COMP-6`: **整數** > `ulong`。

- PICTURE 子句的 `NumericDecoder` 不再「**根據 PIC 和 value 決定最佳型別**」，現在改為「**根據 PIC 決定最佳型別**」。
    - 之前有點搞混`資料型態`與`資料內容`的主從關係。預期結果應該是從 PICTURE 子句的定義觀察出來，而不是資料本身決定。

### Refact
- 重構 PICTURE 子句中 **Numeric** 解碼的 `Mapper`。
    - 讓 `DISPLAY` `COMP-3` `COMP-6` 共用同一個 CLR 轉換規則。

- PICTURE 子句原本的 `Decoder` / `Encoder` 資料夾整併為 `Codec`。
    - 既有分層模式將導致目錄層級過深與模組分散，影響程式碼導覽效率。
    - 調整統一後，集中管理編解碼邏輯，使結構更扁平化並利於未來擴充。

<br><br>

## [26.11.4] – 2026-02-13

### Improved
- 加速 Numeric Decoder/Encoder 計算。
    | Version | Method                | Mean      | Error     | StdDev    |
    |---------|---------------------- |----------:|----------:|----------:|
    | 26.11.1 | Warpper_Write_Decimal | 23.445 μs | 0.0766 μs | 0.0717 μs |
    | 26.11.2 | Warpper_Write_Decimal | 18.889 μs | 0.1028 μs | 0.0961 μs |
    | 26.11.4 | Warpper_Write_Decimal |  9.463 μs | 0.0331 μs | 0.0276 μs |

### Changed
- 將 Numeric Encoder 內暫存的資料 `NumericValue` 改為 `NumericMeta`。

<br><br>


## [26.11.3] – 2026-02-12

### Added
- 新增 `COMP-6` 的支援。
    - 支援的 COBOL 資料型態為 `Unsigned Packed Decimal`，對應到 C# 的 `ulong`。

- 新增對 `Boolen` 的**語意**解析。
    - 對應到 C# 的 `bool`。
    - 當 `PICTURE` 子句採用**語意** ***`PicSemantic.Boolean`*** 解析時：
        - 符號宣告為 `A(1)` / `X(1)` 且 `VALUE` 為 `"Y"` / `"N"` 時，會將其為**布林值**。
        - 符號宣告為 `9(1)` 且 `VALUE` 為 `1` / `0` 時，會將其轉換為**布林值**。

### Changed
- 修正 `COMP-4` 的功能定位
    - 目前等同於 `COMP` (`Binary`) 的處理方式。

<br><br>


## [26.11.2] – 2026-02-10

### Improved
- 對 PICTURE 子句內的 Decimal 編碼過程效能改善。
    | Version | Method                | Mean      | Error     | StdDev    |
    |---------|---------------------- |----------:|----------:|----------:|
    | 26.11.1 | Warpper_Write_Decimal | 23.445 μs | 0.0766 μs | 0.0717 μs |
    | 26.11.2 | Warpper_Write_Decimal | 18.889 μs | 0.1028 μs | 0.0961 μs |

- 重構後的 **PicEncoder** 移除 `CobMeta` 過渡物件。
    - 與 **PicDecoder** 的架構看齊：先處理**語意資料**，再處理基本的 COBOL 資料格式。
    - 使用 **NumericEncoder** 還是需要中繼資料 `NumericValue`，但是資料改用 `ReadOnlyMemory<byte>` 保存，減少轉換開銷。

<br><br>

## [26.11.1] – 2026-02-09

### Bugfix
- 修正 Forge 在 CLR 型別轉錯的問題。

<br><br>

## [26.11.0] – 2026-02-09

### ⚠️ Breaking Changes
- ❌ `CbSerDes` 不再是主要使用功能
    - **CbSerDes** 已標記為 `Obsolete`，不再作為建議的 `Copybook` 操作與序列化介面。
    - **namespace** 從 `GetThePicture.Copybook.SerDes` 改為 `GetThePicture.Obsolete.SerDes`。

- ✅ 新增 Warpper 相關功能，並成為核心使用介面
    - 更明確的資料記憶體映射
    - 與 Copybook 欄位一一對應的強型別屬性
    - 可預期的欄位存取行為

- ✅ 新增 CLI 應用程式子專案：**Forge**
    - 用於協助快速轉換 `Copybook` 成為 `CbWarpper` 的子類別。

<br><br>

## [26.10.3] – 2026-02-07

### Added
- Benchmarks

<br><br>

## [26.10.2] – 2026-02-06

### Changed
- 重新編排 README.md 的章節順序。

<br><br>

## [26.10.1] – 2026-02-06

### Added
- 新增版本號資訊。 
    - Version.Informational

<br><br>

## [26.10.0] – 2026-02-06

### Changed
- ⚠️ 移除Console專案: `Copycat`。
    - `Copycat` 會成為獨立專案，讓這邊可以維持單純的 Library 開發。

<br><br>

## [26.9.0] – 2026-02-06

### Improved
- **PICTURE Clause** 改用專屬的 Lexer + Parser 來處理 Symbols 的語法。
    - 可以處理類似 `XXX(10)X` = X(**13**) 這種混和型的長度定義。
    - 目前先支援常用的 `A` / `X` / `S` / `9`。
    - Symbols 字串最大長度 `50` 為限。

<br><br>

## [26.8.1] – 2026-02-05

### Bugfix
- 修正 Copycat 的編譯錯誤

<br><br>

## [26.8.0] – 2026-02-05

### Added
- 新增 `CbStorage` 與對應處理的 `CbResolver`。
    - 用於處理 Copybook **記憶體位址**映射的相關功能。
- 測試專案增加 **Byte 顯示工具**，用於 `SerDes` 除錯時顯示 buffer 內容。

### Changed
- 將 Ir 更名為 `Layout`。
    - `中間語言（Intermediate language）`這個名詞缺足夠的的閱讀理解。
    - 原本的 `CbSchema` 亦改成 `CbLayout`，貼近直觀上的描述。
- ⚠️ 使用 Provider 作為 CbSerDes 的輸入，不再使用CbSchema。

### Improved
- 重構 Deserializer / Serializer 
    - 改用類似**記憶體位址**映射的方式存取資料，簡化原本遞迴內比較複雜的處理。

<br><br>

## [26.7.0] – 2026-02-02

### Added
- 新增 `REDEFINES` 解析處理
    - `REDEFINES` 僅支援目標為 `ElementaryDataItem` 的同層級欄位。
    - 目前在 C# 轉換輸出中**僅產生說明註解**，不生成實際的屬性或欄位對應程式碼。

<br><br>

## [26.6.1] – 2026-02-02

### Bugfix
- 修正 Deserialize OCCURS 子句時在 ReadElementaryDataItem 內的錯誤。

<br><br>

## [26.6.0] – 2026-02-01

### Added
- 新增 **Level 66 RENAMES**  子句解析支援 (有限度支援)。

<br><br>

## [26.5.0] – 2026-01-30

### Added
- 新增 **Level 88（Condition Name）** 解析處理。
- .NET 程式碼產出功能中，Level 88 會轉換為 布林屬性（boolean properties），盡量看齊 COBOL 語意。
- 支援以下 Level 88 語法：
	- `VALUE`（單一值）
	- `VALUES`（多個值）
	- `VALUE ... THRU ...`（範圍條件）
- 支援 `OCCURS` 子句內的 Level 88 語法。

<br><br>

## [26.4.1] – 2026-01-29

### Changed
- `README.md` 增加開發開發需求說明。

<br><br>

## [26.4.0] – 2026-01-29

### Added
- 新增Console專案: `Copycat`，用於產出 C# 用的資料物件 (sealed class)。

### Improved
- 修正 `反序列化 (Deserialize)` COBOL資料時，沒有檢查資料長度是否與Copybook長度符合的問題。
- 改善 `SerDes` 功能相關程式碼閱讀性。

<br><br>

## [26.3.0] – 2026-01-26

### Added
- 新增 Copybook 的 `Serializer` 與 `Deserializer` 功能。

### Changed
- 移除 `Reader` 改為 `Compiler`。
- 移除 `Writer`。
    - 原為除錯用的工具，與現有 `Dump` 功能雷同。
    - 後續將由 Copybook 的 `Deserializer` 解手處理格式轉換的功能。

<br><br>

## [26.2.0] – 2026-01-25

### Added
- 加入 `Writer` 供 `Document` 後續可支援多種格式輸出。
- `Writer` 支援 `JSON` 格式輸出。

### Improved
- `GroupItem` 與 `ElementaryItem` 支援 COBOL 2002 的 `Floating comment` (`*>`) 解析處理。

<br><br>

## [26.1.0] – 2026-01-24

### Added
- 引入 `Document` 作為整個 Copybook 的根節點，統一管理所有 top-level 項目。
- 支援多層次結構：
    - `GroupItem` 可以包含子節點 (`Children`)。
    - `ElementaryItem` 表示基本資料欄位，無子節點。
    - 遞迴解析任意層次的 group/elementary 結構。

### Improved
- 解析器改進：
    - `ParseDataItem` 支援遞迴解析子項目。
    - 自動判斷下一個項目層級，正確建立 parent-child 關係。
- 資料項目支援 `PIC`、`OCCURS` clause，可擴充 `VALUE` 或其他 clause。
- IR (Intermediate Representation) 完整樹狀結構，可用於後續轉 JSON、驗證或其他分析。

<br><br>

## [26.0.0] – 2026-01-21

### Added
- 初步支援 PIC clause 編碼與解碼 (PIC codec)：
    - `9(n)`、`X(n)` 基本型態轉換。
    - DISPLAY 格式與字串轉 byte 支援。
- 建立 `ElementaryItem` 支援 PIC 資訊。
- 測試方法與範例：
    - 驗證 PIC 編碼與解碼正確性。
    - 支援單一資料項目轉 byte 陣列。
  
### Improved
- 初步錯誤處理：
    - 當輸入字串長度與 PIC 定義不符時，拋出例外。
- 支援 CP950 字元編碼的字串轉 byte 功能。
