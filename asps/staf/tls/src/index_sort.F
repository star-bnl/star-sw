* $Id: index_sort.F,v 1.1 1998/03/24 00:42:02 fisyak Exp $
* $Log: index_sort.F,v $
* Revision 1.1  1998/03/24 00:42:02  fisyak
* Clean
*
      INTEGER FUNCTION Index_Sort( nrows, el1, el2, index, ind_size )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C Same routine as Index_Sort except el1 and el2 are I*4
C INPUT  VARIABLES:
      INTEGER nrows             ! Length of el1 array.
      INTEGER el1(*)            ! Element to sort on (in first row of table)
      INTEGER el2               ! Element from second row of table.
      INTEGER ind_size          ! Allocated size of index.
C
C OUTPUT VARIABLES:
      INTEGER index(ind_size)   ! Order to access Table in.
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
C
      INTEGER tls_loc
      INTEGER I, IR, J, L, IndxT
      INTEGER len
      INTEGER Q
C----------------------------------------------------------------------------
C
C
      IF (ind_size.LT.nrows) THEN
          Index_Sort = tls_sort_is_short_cv
          RETURN
          END IF
      DO 1 J = 1,nrows
    1    Index(J) = J
C
      L  = 1 + nrows/2
      IR = nrows
      len = (tls_loc(el2) - tls_loc(el1(1)))/4
   10 CONTINUE
      IF (L.GT.1) THEN
          L = L - 1
          IndxT = index(L)
          Q = el1(1+len*(Indxt-1))
        ELSE
          IndxT = index(IR)
          Q = el1(1+len*(Indxt-1))
          index(IR) = index(1)
          IR = IR - 1
          IF (IR.EQ.1) THEN
              index(1) = IndxT
              Index_Sort = tls_sort_normal_cv
              RETURN
              END IF
        END IF
      I = L
      J = L + L
   20 IF (J.LE.IR) THEN
          IF (J.LT.IR) THEN
              IF (el1(1+len*(index(J)-1)).LT.el1(1+len*(index(J+1)-1)))
     +              J=J+1
              END IF
          IF (Q.LT.el1(1+len*(index(J)-1))) THEN
              index(I) = index(J)
              I = J
              J = J + J
            ELSE
              J = IR + 1
            END IF
          GO TO 20
          END IF
      index(I) = IndxT
      GO TO 10
      END
