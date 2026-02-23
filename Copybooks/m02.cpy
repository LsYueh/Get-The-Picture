      * 申購/買回明細申報檔
      * 檔案長度：150
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..      
       01  M02-TRAN-CODE           PIC X(01).  *> 異動碼
       01  M02-ETF-ID              PIC X(06).  *> ETF代號   
       01  M02-BROKER-ID           PIC X(04).  *> 券商代表號
       01  M02-TX-DATE             PIC 9(08).  *> 申請日(西曆)
       01  M02-SEQNO               PIC X(03).  *> 流水號    
       01  M02-ACNT-BROKER         PIC X(04).  *> 開戶券商代號
       01  M02-ACNT-NO             PIC 9(07).  *> 申請人帳號  
       01  M02-STKNO               PIC X(06).  *> 股票代號    
       01  M02-NORMAL-STOCK-NOS    PIC 9(10).  *> 庫存部位       
       01  M02-BORROW-STOCK-NOS    PIC 9(10).  *> 借券部位
       01  M02-T1-STOCK-NOS        PIC 9(10).  *> T-1日淨入庫部位
       01  M02-T-STOCK-NOS         PIC 9(10).  *> T日淨入庫部位  
       01  M02-LACK-STOCK-NOS      PIC 9(10).  *> 短缺部位       
       01  M02-CASH-IN-LIEU        PIC X(01).  *> 現金替代記號
       01  M02-LIEU-REASON         PIC X(01).  *> 替代原因    
       01  M02-QFII-AVB-STOCK-NOS  PIC 9(10).  *> 外資可贖股數
       01  M02-ARBITRAGE-NOS       PIC 9(10).  *> 套利賣空部位
       01  M02-ERROR-CODE          PIC X(02).  *> 錯誤代碼(空白) 
       01  M02-STOCK-NOS-5         PIC 9(10).  *> 前日申購/買回部位
       01  FILLER              PIC X(27).
       
       