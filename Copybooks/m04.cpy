      * 檔案長度：200
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7.. 
       01  M04. *> 發行人回覆資料檔
           05 PROC-DATE            PIC 9(08).  *> 資料日(西曆)
           05 ETF-ID               PIC X(06).  *> ETF代號
           05 BROKER-ID            PIC X(04).  *> 券商代表號
           05 TX-DATE              PIC 9(08).  *> 申請日(西曆)
           05 SEQNO                PIC X(03).  *> 流水號
           05 RESULT               PIC X(01).  *> 處理結果
           05 FAIL-REASON          PIC X(02).  *> 失敗原因
           05 CASH-DIF-AMOUNT-S    PIC X(01).  *> 現金差額 S9(09)
           05 CASH-DIF-AMOUNT-9    PIC 9(09).  *> 現金差額 S9(09)
           05 MARGIN-AMOUNT        PIC 9(09).  *> 應收保證金
           05 CASH-LIEU-AMOUNT-S   PIC X(01).  *> 現金替代金額 S9(09)
           05 CASH-LIEU-AMOUNT-9   PIC 9(09).  *> 現金替代金額 S9(09)
           05 FILLER               OCCURS 3 TIMES. *> (申請人)
               10 ACNT-BROKER          PIC X(04).  *> 開戶券商代號
               10 ACNT-NO              PIC 9(07).  *> 申請人帳號
               10 ETF-SHR              PIC 9(18).  *> 單位數
           05 APPLY-FEE            PIC 9(08).  *> 申購買回手續費
           05 MANAGEMENT-CHARGE    PIC 9(08).  *> 行政處理費
           05 FILLER               PIC X(36).
