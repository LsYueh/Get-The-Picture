# Changelog

## [26.6.0] – 2026-02-01

### 新增
- 新增 **Level 66 RENAMES**  子句解析支援 (有限度支援)。

<br><br>

## [26.5.0] – 2026-01-30

### 新增
- 新增 **Level 88（Condition Name）** 解析處理。
- .NET 程式碼產出功能中，Level 88 會轉換為 布林屬性（boolean properties），盡量看齊 COBOL 語意。
- 支援以下 Level 88 語法：
	- `VALUE`（單一值）
	- `VALUES`（多個值）
	- `VALUE ... THRU ...`（範圍條件）
- 支援 `OCCURS` 子句內的 Level 88 語法。

<br><br>

## [26.4.1] – 2026-01-29

### 變更
- `README.md` 增加開發開發需求說明。

<br><br>

## [26.4.0] – 2026-01-29

### 新增
- 新增Console專案: `Copycat`，用於產出 C# 用的資料物件 (sealed class)。

### 改進
- 修正 `反序列化 (Deserialize)` COBOL資料時，沒有檢查資料長度是否與Copybook長度符合的問題。
- 改善 `SerDes` 功能相關程式碼閱讀性。

<br><br>

## [26.3.0] – 2026-01-26

### 新增
- 新增 Copybook 的 `Serializer` 與 `Deserializer` 功能。

### 移除
- 移除 `Reader` 改為 `Compiler`。
- 移除 `Writer`。
  - 原為除錯用的工具，與現有 `Dump` 功能雷同。
  - 後續將由 Copybook 的 `Deserializer` 解手處理格式轉換的功能。

<br><br>

## [26.2.0] – 2026-01-25

### 新增
- 加入 `Writer` 供 `Document` 後續可支援多種格式輸出。
- `Writer` 支援 `JSON` 格式輸出。

### 改進
- `GroupItem` 與 `ElementaryItem` 支援 COBOL 2002 的 `Floating comment` (`*>`) 解析處理。

<br><br>

## [26.1.0] – 2026-01-24

### 新增
- 引入 `Document` 作為整個 Copybook 的根節點，統一管理所有 top-level 項目。
- 支援多層次結構：
  - `GroupItem` 可以包含子節點 (`Children`)。
  - `ElementaryItem` 表示基本資料欄位，無子節點。
  - 遞迴解析任意層次的 group/elementary 結構。

### 改進
- 解析器改進：
  - `ParseDataItem` 支援遞迴解析子項目。
  - 自動判斷下一個項目層級，正確建立 parent-child 關係。
- 資料項目支援 `PIC`、`OCCURS` clause，可擴充 `VALUE` 或其他 clause。
- IR (Intermediate Representation) 完整樹狀結構，可用於後續轉 JSON、驗證或其他分析。

<br><br>

## [26.0.0] – 2026-01-21

### 新增
- 初步支援 PIC clause 編碼與解碼 (PIC codec)：
  - `9(n)`、`X(n)` 基本型態轉換。
  - DISPLAY 格式與字串轉 byte 支援。
- 建立 `ElementaryItem` 支援 PIC 資訊。
- 測試方法與範例：
  - 驗證 PIC 編碼與解碼正確性。
  - 支援單一資料項目轉 byte 陣列。
  
### 改進
- 初步錯誤處理：
  - 當輸入字串長度與 PIC 定義不符時，拋出例外。
- 支援 CP950 字元編碼的字串轉 byte 功能。
