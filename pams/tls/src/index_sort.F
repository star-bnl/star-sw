* $Id: index_sort.F,v 1.4 1998/08/17 14:39:22 fisyak Exp $
* $Log: index_sort.F,v $
* Revision 1.4  1998/08/17 14:39:22  fisyak
* clean up
*
* Revision 1.3  1998/08/16 16:59:00  perev
* Increase stack 20->40
*
* Revision 1.2  1998/08/13 02:08:57  perev
* cleanup+speedup+leaks
*
* Revision 1.1  1998/03/24 00:42:02  fisyak
* Clean
*
      INTEGER FUNCTION Index_Sort( nrows, el1, el2, index, ind_size )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
      INTEGER nrows             ! Length of el1 array.
      INTEGER el1(*)            ! Element to sort on (in first row of table)
      INTEGER el2(1)            ! Element from second row of table.
      INTEGER ind_size          ! Allocated size of index.
C
C OUTPUT VARIABLES:
      INTEGER index(*)          ! Order to access Table in.
      INTEGER len
C
C        Return Values
C           tls_sort_normal_cv (S):    Successful.
C           tls_sort_is_short_cv (E): index array not long enough.
C
C   Comments: This function is typically called once per module for
C             each table that should be sorted. After the call we have;
C
C             Table(index(j)).real <= Table(index(k)).real   for   j < k.
C
C             The integer or real version of the routine must be called
C             depending on if the column to be sorted on is integer or real.
C             When these routines are implemented in Fortran 90 the
C             generic interface will be called Index_Sort.
C
C             This implementation uses a heap sort algorithm.
C             Quicksort is generally faster unless the table is already
C             nearly sorted in which case it is very slow.
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,   8/30/91
C                       Taken from Numerical Recipes.
C     Modified         11/ 4/93 More convenient for STAR tables.
C     Modified         08/11/98 CERN algorithm (INTSOR) implemented.
C                      speedup 3 times for non opt compiler
C

      INTEGER loc,bytes,j
C

      bytes = abs(loc(el1(2))-loc(el1(1)))
      len =   abs((loc(el2) - loc(el1(1)))/bytes)
      index(1) = 1
      do  j=2,NROWS     
        index(j) = index(j-1) + len
      enddo
      call  TSORTI_IDX (el1, index, NROWS)
      do  j=1,NROWS     
        index(j) = (index(j)-1)/len+1
      enddo
      Index_Sort  = tls_sort_normal_cv
      return
      end

*DECK, TSORTI_IDX.
      SUBROUTINE TSORTI_IDX (A, IND, N)
************************************************************************
*                                                                      *
*  Sort the indexes of 1-dimensional integer array           VP 800101 *
*  IND(1),...,IND(N) by increasing value A(IND(*))                     *
*                                                                      *
*  Array IND supposed to be prefilled                                  *
*                                                                      *
************************************************************************

      INTEGER A(*),X
      INTEGER        W, R, LT(40), RT(40), IND(N)
*--------------------------------------------------------------------------

      LEVEL = 1
      LT(1) = 1
      RT(1) = N
  100 L = LT(LEVEL)
      R = RT(LEVEL)
      LEVEL = LEVEL-1
  120 IF (R.GT.L)               GO TO 140
      IF (LEVEL)                999,999,100

  140 I = L
      J = R
      M = (L+R)/2
      X = A(IND(M))
  160 IF (A(IND(I)) .GE. X)         	GO TO 180

      I = I+1
      GO TO 160
  180 IF (A(IND(J)) .le. X)         	GO TO 200

      J = J-1
      GO TO 180

  200 IF (I.GT.J)               GO TO 220
      W = IND(I)
      IND(I) = IND(J)
      IND(J) = W
      I = I+1
      J = J-1
      IF (I.LE.J)               GO TO 160

  220 LEVEL = LEVEL+1
      IF ((R-I).GE.(J-L))       GO TO 240
      LT(LEVEL) = L
      RT(LEVEL) = J
      L = I
      GO TO 120
  240 LT(LEVEL) = I
      RT(LEVEL) = R
      R = J
      GO TO 120
  999 END
