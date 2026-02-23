      * 檔案長度：300
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..      
       01  M01. *> 申購/買回彙總申報檔
           05  TRAN-CODE           PIC X(01).  *> 異動碼          
           05  ETF-ID              PIC X(06).  *> ETF代號         
           05  BROKER-ID           PIC X(04).  *> 券商代表號      
           05  TX-DATE             PIC 9(08).  *> 申請日(西曆)
           05  SEQNO               PIC X(03).  *> 流水號 
           05  TX-KIND             PIC X(01).  *> 交易種類
           05  APPLICATION-UNITS   PIC 9(03).  *> 申請基數
           05  STATE               PIC X(01).  *> 註記(空白)
           05  BANK-ID             PIC 9(03).  *> 買回時匯款銀行代號
           05  RM-ACNT             PIC X(16).  *> 買回時匯款帳號
           05  APPLIER-NUMBER      PIC 9(01).  *> 申請人數目
           05  FILLER              OCCURS 3 TIMES. *> (申請人)
               10  ACNT-BROKER         PIC X(04).  *> 開戶券商代號
               10  ACNT-NO             PIC 9(07).  *> 申請人帳號
               10  KEEP-ACNT           PIC X(11).  *> 申請人保管銀行帳號
               10  ID-CODE             PIC X(03).  *> 身份碼
               10  CASH-ASSIGN         PIC X(01).  *> 現金差額收取人
               10  MERGE-ASSIGN        PIC X(01).  *> 零股整合帳戶
           05  APPLY-FEE           PIC 9(08).  *> 申購買回手續費
           05  MANAGEMENT-CHARGE   PIC 9(08).  *> 行政處理費 
           05  ERROR-CODE          PIC X(02).  *> 錯誤代碼(空白)
           05  TX-CASH             PIC X(01).  *> 現金申贖Y/ ” ”
           05  AMOUNT              PIC 9(18).  *> 現金申贖金額
           05  RM-ACNT-NAME        PIC X(60).  *> 匯款帳戶名
           05  RM-ACNT-ID          PIC X(10).  *> 匯款帳戶ID
           05  FILLER              PIC X(65).
       