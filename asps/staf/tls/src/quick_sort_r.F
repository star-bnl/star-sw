* $Id: quick_sort_r.F,v 1.6 1998/09/21 19:30:48 perev Exp $
* $Log: quick_sort_r.F,v $
* Revision 1.6  1998/09/21 19:30:48  perev
* Simple tests added
*
* Revision 1.5  1998/09/20 00:52:34  fisyak
* Remoe back
*
* Revision 1.4  1998/09/19 13:00:01  fisyak
* add tls_uswop
*
* Revision 1.3  1998/08/16 16:59:01  perev
* Increase stack 20->40
*
* Revision 1.2  1998/08/13 02:08:59  perev
* cleanup+speedup+leaks
*
* Revision 1.1  1998/03/24 00:42:04  fisyak
* Clean
*
      INTEGER FUNCTION Quick_Sort_r( nrows, el1, el2, table )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C
C Actual sort of table assuming element to be sorted on is integer.
C Taken from Numerical Recipes.
C        Return Values
C           tls_sort_normal_cv (S):         Successful.
C
C Inputs:
      INTEGER nrows         ! Number of input rows.
      REAL    el1(*)        ! Element to sort on (from first row of table)
      REAL    el2           ! Element to sort on (from second row of table)
      REAL    table(*)      ! table to be sorted.
C
      INTEGER loc,bytes,len,itst
C

      bytes = abs(loc(table(2))-loc(table(1)))
      len   = abs((loc(el2) - loc(el1(1)))/bytes)
      itst  = abs((loc(el1) - loc(table(1))))/bytes
      if (itst .ge. len) then
         write(*,*) 'Quick_Sort_r ***ERROR** Wrong Arguments'
         Quick_Sort_r = -1
         return
      endif
      call  TSORTR (table, len, NROWS, el1)
      Quick_Sort_r = tls_sort_normal_cv
      return
      end

CDECK  ID>, TSORTR.
      SUBROUTINE TSORTR (A, NROW, NCOL, WORD)
C
C   SORT THE TWO-DIMENSIONAL real ARRAY A(JW,1),...,A(JW,N) BY
C   JW > 0 BY INCREASING VALUE OF A( JW,*)
C   JW < 0 BY DECREASING VALUE OF A(-JW,*)
C Routine is prodused from INTSOR (VP) see below
C
C CERN PROGLIB# M103    INTSOR          .VERSION KERNFOR  3.15  820113
C ORIG. 29/04/78
C
C   SORT THE ONE-DIMENSIONAL INTEGER ARRAY A(1),...,A(N) BY
C   INCREASING VALUES
C
C-    PROGRAM  M103  TAKEN FROM CERN PROGRAM LIBRARY,  29-APR-78
C
      REAL           A(NROW,NCOL),WORD(NROW,NCOL),X,W
      INTEGER        LT(40),RT(40)
      INTEGER        R,RT
C
      if(NCOL.le.1) return
      do I=1,NCOL-1
        if (WORD(1,I).GT.WORD(1,I+1)) go to 1
      enddo
      return
      
   1  N = NCOL
      LEVEL = 1
      LT(1) = 1
      RT(1) = N
   10 L = LT(LEVEL)
      R = RT(LEVEL)
      LEVEL = LEVEL-1
   20 IF (R.GT.L)               GO TO 200
      IF (LEVEL)                99,99,10
C
C   SUBDIVIDE THE INTERVAL L,R
C     L : LOWER LIMIT OF THE INTERVAL (INPUT)
C     R : UPPER LIMIT OF THE INTERVAL (INPUT)
C     J : UPPER LIMIT OF LOWER SUB-INTERVAL (OUTPUT)
C     I : LOWER LIMIT OF UPPER SUB-INTERVAL (OUTPUT)
C
  200 I = L
      J = R
      M = (L+R)/2
      X = WORD(1,M)
  220 IF (WORD(1,I).GE.X)         GO TO 230
      I = I+1
      GO TO 220
  230 IF (WORD(1,J).LE.X)         GO TO 231
      J = J-1
      GO TO 230
C
  231 IF (I.GT.J)               GO TO 232
      DO JJ = 1,NROW
        W   = A(JJ,I)
        A(JJ,I) = A(JJ,J)
        A(JJ,J) = W
      ENDDO
      I = I+1
      J = J-1
      IF (I.LE.J)               GO TO 220
C
  232 LEVEL = LEVEL+1
      IF ((R-I).GE.(J-L))       GO TO 30
      LT(LEVEL) = L
      RT(LEVEL) = J
      L = I
      GO TO 20
   30 LT(LEVEL) = I
      RT(LEVEL) = R
      R = J
      GO TO 20
C
C
   99 RETURN
      END

