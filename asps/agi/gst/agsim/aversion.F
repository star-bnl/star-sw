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
     +'@(#)* Advanced Geant Inteface   2.00/03   C: 25/03/2000  15.39.00
     +'/
*KEND.
      QFTITLCH=VIDQQ(5:60)
*KEEP,DATEQQ.
      IDATQQ = 20000325
*KEEP,TIMEQQ.
      ITIMQQ =   1539
*KEEP,VERSQQ.
      VERSQQ = ' 2.00/03'
      IVERSQ =  20003
*KEND.
      END
 
