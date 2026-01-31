# Copybook `RECORD KEY` Clause

在 COBOL 中，針對 `ISAM`（索引循序存取方法，現代大型主機常指 VSAM KSDS）進行隨機搜尋（`Random Search`），主要透過設定鍵值並執行 READ 指令來實現。  

> ⚠️ `RECORD KEY` 必須指向一個具有實體定義的資料項目（即 Level 01 到 49 之間的欄位）。  

操作步驟：
1. 環境部定義 (ENVIRONMENT DIVISION)  
    在 FILE-CONTROL 中，必須將存取模式設為 RANDOM 或 DYNAMIC。 
        - RANDOM：僅允許隨機存取。
        - DYNAMIC：允許在程式中切換隨機與循序存取。 

    ```cobol
    ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        SELECT EMP-FILE ASSIGN TO "EMPMASTER.DAT"
            ORGANIZATION IS INDEXED
            ACCESS MODE  IS RANDOM        *> 指定隨機存取
            RECORD KEY   IS EMP-ID        *> 對應 Copybook 內的欄位
            FILE STATUS  IS WS-FS.
    ```

2. 資料部定義 (DATA DIVISION)  
    確保 `RECORD KEY` 對應的欄位在 FD 中定義明確。
    ```cobol
    * EMPREC.CPY - 員工主檔結構
    01 EMP-RECORD.
        05 EMP-ID           PIC X(10).    *> 這就是 RECORD KEY 所指的欄位
        05 EMP-NAME         PIC X(30).
        05 EMP-DEPT         PIC X(04).
        05 EMP-SALARY       PIC 9(07)V99.
    ```

    ```cobol
    DATA DIVISION.
    FILE SECTION.
    FD EMP-FILE.
        COPY "EMPREC.CPY".  *> 系統會在此展開 01 EMP-RECORD 及其下的 EMP-ID

    WORKING-STORAGE SECTION.
    01  WS-FS             PIC XX.        *> 檔案狀態碼 (File Status)
        88  FS-SUCCESS    VALUE "00".    *> 成功狀態
        88  FS-NOT-FOUND  VALUE "23".    *> 找不到紀錄
    ```

3. 程序部 (PROCEDURE DIVISION)  
    執行隨機搜尋的標準流程如下：  
    1. 移動鍵值：將目標搜尋值存入 `RECORD KEY` 欄位。
    2. 執行 READ：使用帶有鍵值的 `READ` 指令。
    3. 錯誤處理：檢查 `INVALID KEY` 或 `FILE STATUS`。

    ```cobol
    OPEN INPUT EMP-FILE.
    MOVE "ID0001" TO EMP-ID.    *> 1. 設定要搜尋的目標 Key 值
    READ EMP-FILE               *> 2. 執行隨機讀取
        INVALID KEY
            DISPLAY "錯誤：找不到員工編號 " EMP-ID
        NOT INVALID KEY
            DISPLAY "讀取成功！"
            DISPLAY "姓名：" EMP-NAME
            DISPLAY "部門：" EMP-DEPT
            DISPLAY "薪資：" EMP-SALARY
    END-READ.
    CLOSE EMP-FILE.
    ```

<br>

從以上操作流程可知， Copybook 本身不像 SQL 的 `DDL (Data Definition Language)` 一樣有能力定義索引、主鍵等資料。  
但是在實務上還是可以使用 `Level 66` 子句，透過語意的方式添加「`邏輯上的鍵（logical key）`」。
```cobol
66 EMP-KEY RENAMES EMP-ID THRU EMP-DEPT.
```

<br>

1. 用 Level 66 對應的結構來搜尋
    ```cobol
    ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        *> 定義一個沒有索引的循序檔
        SELECT EMP-FILE ASSIGN TO "EMPMASTER.DAT"
            ORGANIZATION IS SEQUENTIAL
            ACCESS MODE  IS SEQUENTIAL
            FILE STATUS  IS WS-FS.
    ```

    ```cobol
    DATA DIVISION.
    FILE SECTION.
    FD EMP-FILE.
        COPY "EMPREC.CPY".

    WORKING-STORAGE SECTION.
    *> 主要資料結構
    01  WS-EMP-RECORD.
        05  WS-EMP-ID          PIC X(10) VALUE SPACES.
        05  WS-EMP-NAME        PIC X(30) VALUE SPACES.
        05  WS-EMP-DEPT        PIC X(04) VALUE SPACES.
        05  FILLER             PIC X(36). *> 其他不參與搜尋比對的欄位放後面

    *> 定義 Level 66 邏輯鍵 (ID + NAME 總共 40 Bytes)
    66  WS-EMP-KEY RENAMES WS-EMP-ID THRU WS-EMP-DEPT.

    *> 搜尋目標 (必須與 Level 66 長度一致)
    01  WS-SEARCH-TARGET       PIC X(40).

    *> 狀態與控制變數
    01  WS-FS                  PIC XX.
        88  FS-SUCCESS         VALUE "00".
        88  FS-EOF             VALUE "10".
    01  WS-FOUND-FLAG          PIC X VALUE "N".
        88  RECORD-FOUND       VALUE "Y".
    ```

<br>

2. 用 66 進行 Sequential Search (循序搜尋)

    ```cobol
    PROCEDURE DIVISION.
        MOVE SPACES TO WS-SEARCH-TARGET
        MOVE "EMP001" TO WS-SEARCH-TARGET(1:10)
        MOVE "WANG XIAO MING" TO WS-SEARCH-TARGET(11:30)

        OPEN INPUT EMP-FILE.
        
        PERFORM UNTIL FS-EOF OR RECORD-FOUND
            READ EMP-FILE INTO WS-EMP-RECORD
                AT END
                    SET FS-EOF TO TRUE
                NOT AT END
                    *> 使用 Level 66 別名進行比對
                    IF WS-EMP-KEY = WS-SEARCH-TARGET
                        SET RECORD-FOUND TO TRUE
                    END-IF
            END-READ
        END-PERFORM.

        IF RECORD-FOUND
            DISPLAY "SUCCESS: FOUND EMPLOYEE!"
            DISPLAY "DEPT: " WS-EMP-DEPT
        ELSE
        
        CLOSE EMP-FILE.
        STOP RUN.
    ```

<br>
