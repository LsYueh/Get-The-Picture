|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
      * EMPREC.CPY - 員工主檔結構
       01  EMP-RECORD.
           05 EMP-ID           PIC X(10).
           05 EMP-NAME         PIC X(30).
           05 EMP-DEPT         PIC X(04).
           05 EMP-SALARY       PIC 9(07)V99.
       66  EMP-KEY RENAMES EMP-ID THRU EMP-DEPT.