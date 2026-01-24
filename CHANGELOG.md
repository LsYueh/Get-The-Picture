# Changelog

## [26.1.0] – 2026-01-24

### 新增
- 引入 `Document` 作為整個 Copybook 的根節點，統一管理所有 top-level 項目。
- 支援多層次結構：
  - `GroupItem` 可以包含子節點 (`Children`)。
  - `ElementaryItem` 表示基本資料欄位，無子節點。
  - 遞迴解析任意層次的 group/elementary 結構。
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
