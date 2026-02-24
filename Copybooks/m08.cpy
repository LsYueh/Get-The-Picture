      * 檔案長度：100
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7.. 
       01  M08. *> 補券資料檔 (查詢結果)
           05  PROC-DATE           PIC 9(08).  *> 處理日期  
           05  ETF-ID              PIC X(06).  *> ETF代號   
           05  BROKER-ID           PIC X(04).  *> 券商代表號
           05  TX-DATE             PIC 9(08).  *> 原申請日  
           05  SEQNO               PIC X(03).  *> 流水號    
           05  ACNT-BROKER         PIC X(04).  *> 開戶券商代號
           05  ACNT-NO             PIC 9(07).  *> 申請人帳號  
           05  STKNO               PIC X(06).  *> 股票代號    
           05  TODAY-NET-NOS       PIC 9(10).  *> 本日淨買進部位
           05  BORROW-STOCK-NOS    PIC 9(10).  *> 借券部位     
           05  TODAY-NET-NOS-A     PIC 9(10).  *> 本日淨買進部位
           05  BORROW-STOCK-NOS-A  PIC 9(10).  *> 借券部位      
           05  FILLER              PIC X(14).         
