* $Id: tls_zero_row.F,v 1.1 1998/03/24 00:42:14 fisyak Exp $
* $Log: tls_zero_row.F,v $
* Revision 1.1  1998/03/24 00:42:14  fisyak
* Clean
*
      INTEGER FUNCTION tls_Zero_Row( el1, el2 )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C        Inputs
      INTEGER el1(*)    ! First element of row zero.
      INTEGER el2       ! First element in second row.
 
C        Outputs
C     None; memory starting at el1 going to el2 is zeroed.
 
C        Return Values
C           tls_sort_normal_cv (S):      Successful.
C           tls_sort_id_short_cv (E):    el2 before el1 in memory.
 
C        Comments: Normal call would look something like:
C                  CALL tls_Zero_Row( Table(i), Table(i+1) )

C                  This will set all elements of row i to zero.
C                  The current implementation assumes the row is a mutiple
C                  of 4 bytes long. This could be easily changed to
C                  multiples of 2 bytes, at the possible cost of execution
C                  speed.
C
C
      INTEGER tls_loc ! Function to get memory location of variable.
      INTEGER rc      ! Return code.
      INTEGER nel     ! Number of elements in row of table
      INTEGER i       ! Do loop index
C
C Loop through table filling index.
      rc = tls_sort_normal_cv
      nel = (tls_loc(el2) - tls_loc(el1(1)))/4
      IF (nel .LE. 0) THEN
          tls_Zero_Row = tls_sort_id_short_cv
          RETURN
          END IF
      DO i = 1,nel
         el1(i) = 0
         END DO
      tls_Zero_Row = rc
      RETURN
      END
