C
C Changes:
C   01-06-2002 (LF) - increase PARI array size from 40 -> 50
C
      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/
