*CMZ :  2.00/00 01/07/99  17.36.35  by  Pavel Nevski
*-- Author :    Pavel Nevski   10/07/96
      SUBROUTINE AVERSION
*KEEP,agcvers.
      COMMON /AGCVERS/IDATQQ,ITIMQQ,IVERSQ,VERSQQ,QFTITLCH
      INTEGER         IDATQQ,ITIMQQ,IVERSQ
      CHARACTER       VERSQQ*8,QFTITLCH*60
*     - - - - - - - - - - - - - - - - - - - -
*KEEP,VIDQQ.
      CHARACTER*68 VIDQQ
      DATA VIDQQ/
     +'@(#)* Advanced Geant Inteface   2.00/00   C: 04/01/2000  19.02.02
     +'/
*KEND.
      QFTITLCH=VIDQQ(5:60)
*KEEP,DATEQQ.
      IDATQQ = 20000104
*KEEP,TIMEQQ.
      ITIMQQ =   1902
*KEEP,VERSQQ.
      VERSQQ = ' 2.00/00'
      IVERSQ =  20000
*KEND.
      END
 
