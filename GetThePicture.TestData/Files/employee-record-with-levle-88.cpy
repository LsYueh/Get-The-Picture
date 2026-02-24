|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01  EMPLOYEE-RECORD.
           05 EMP-STATUS       PIC X.
               88 ACTIVE           VALUE 'A'.
               88 INACTIVE         VALUE 'I'.
               88 ON-LEAVE         VALUE 'L'.
       
           05 EMP-TYPE         PIC X.
               88 FULL-TIME        VALUE 'F'.
               88 PART-TIME        VALUE 'P'.
               88 CONTRACTOR       VALUE 'C'.
       
           05 EMP-LEVEL        PIC 9.
               88 JUNIOR           VALUE 1.
               88 MID              VALUE 2.
               88 SENIOR           VALUE 3.
               88 MANAGER          VALUE 4.
