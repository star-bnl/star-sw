* $Id: search.F,v 1.1 1998/03/24 00:42:05 fisyak Exp $
* $Log: search.F,v $
* Revision 1.1  1998/03/24 00:42:05  fisyak
* Clean
*
      INTEGER FUNCTION Search( nrows, el1, el2, key, ind_r, k_rows )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C Inputs
      INTEGER nrows          ! Number of rows in the table.
      INTEGER el1(*)         ! Element we consider to be a key (in first row)
      INTEGER el2            ! Element we consider to be a key (in first row)
      INTEGER key            ! Key we are searching for.
C Outputs
      INTEGER k_rows         ! Number of keys found. On input this is taken to
                             ! be the dimension of ind_r.
      INTEGER ind_r(k_rows)  ! Indices of rows which contain rkey in the table.
                             ! NOTE THAT ind_r(1) CAN BE 0 IF rkey < el1(1)
                             ! FOR ASCENDING TABLE OR rkey > el1(nrows) FOR
                             ! DESCENDING TABLE.
C
C           
C        Return Values
C           tls_sort_normal_cv (S):      Normal return
C           tls_sort_s_nomatch_cv (I):   key not found in specified column.
C           tls_sort_s_listshort_cv (W): ind_r too short for all rows with key.
C           
C        Comments: These functions use a bisection search algorithm to search
C                  an ordered list (Table) for a given element (key). The
C                  offset into the list is returned. We return the index
C                  ind_r such that
C                      el1(ind_r) <= rkey < el1(ind_r+1) 
C                  if table is sorted in increasing order or
C                      el1(ind_r) > rkey => el1(ind_r+1) 
C                  if the table is sorted in decreasing order.
C
C                  This algorithm assumes that nothing is known about where
C                  key will be found. If the approximate position of key is
C                  known it will be more efficient to use Locate.
C 
C                  When these are implemented in Fortran 90 the generic 
C                  name will be Search.
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,  12/ 4/91
C                       Taken from Numerical Recipes.
C         Modified     11/ 5/93 More convenient for Tables.
C
      INTEGER tls_loc
      INTEGER JLow, JHigh, JM
      INTEGER len, inc, i_row
      LOGICAL Ascend
C-------------------------------------------------------------------------------
C
      JLow   = 0
      JHigh  = nrows + 1
      len    = (tls_loc(el2) - tls_loc(el1(1)))/4
      Ascend = el1(1+len*(nrows-1)).GT.el1(1)
   10 CONTINUE
      IF (JHigh.GT.JLow+1) THEN
          JM = (JHigh+JLow)/2
#ifdef Linux
          IF ((key.GT.el1(1+len*(JM-1))) .EQV. Ascend) THEN
#else
          IF ((key.GT.el1(1+len*(JM-1))) .EQ. Ascend) THEN
#endif
              JLow = JM
            ELSE
              JHigh = JM
            END IF
          GO TO 10
          END IF
C
C  If  Ascend we have el1(Jlow)  < key <= el1(JHigh)
C  If ~Ascend we have el1(Jlow) => key >  el1(JHigh)
      IF (Ascend) THEN
          ind_r(1) = JHigh
          JM  = JHigh
          inc = + 1
        ELSE
          ind_r(1) = JLow
          JM  = JLow
          inc = - 1
        END IF
      IF (el1(1+len*(ind_r(1)-1)).NE.key) THEN
          Search = tls_sort_s_nomatch_cv
          RETURN
        ELSE
          IF (k_rows.GT.1) THEN
              i_row = 2
              JM = JM + inc
              DO WHILE (el1(1+len*(JM-1)).EQ.key)
                 IF (i_row.GT.k_rows) THEN
                    Search  = tls_sort_s_listshort_cv
                    RETURN
                    END IF
                 ind_r(i_row) = JM
                 i_row = i_row + 1
                 JM     = JM + inc
                 END DO
              END IF
        END IF
      Search = tls_sort_normal_cv
      RETURN
      END
