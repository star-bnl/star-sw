* $Id: locate_r.F,v 1.1 1998/03/24 00:42:03 fisyak Exp $
* $Log: locate_r.F,v $
* Revision 1.1  1998/03/24 00:42:03  fisyak
* Clean
*
      INTEGER FUNCTION Locate_r( nrows, el1, el2, rkey, ind_r, k_rows )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C Inputs
      INTEGER nrows          ! Number of rows in the table.
      REAL    el1(*)         ! Element we consider to be a key (in first row)
      REAL    el2            ! Element we consider to be a key (in first row)
      REAL    rkey           ! Key we are searching for.
C Outputs
      INTEGER k_rows         ! Number of keys found. On input this is taken to
                             ! be the dimension of ind_r.
      REAL    ind_r(k_rows)  ! Indices of rows which contain rkey in the table.
                             ! NOTE THAT ind_r(1) CAN BE 0 IF rkey < el1(1)
                             ! FOR ASCENDING TABLE OR rkey > el1(nrows) FOR
                             ! DESCENDING TABLE.
C
C           
C        Return Values
C           tls_sort_normal_cv (S):       Normal return
C           tls_sort_s_nomatch_cv (I):   rkey not found in specified column.
C           tls_sort_s_listshort_cv (W): ind_r too short for all rows with rkey.
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
C                  This algorithm uses ind_r(1) as the starting index
C                  for its search. If the approximate position of key is
C                  known it will be more efficient to use Search.
C 
C                  When these are implemented in Fortran 90 the generic 
C                  name will be Search.
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,  12/ 4/91
C                       Taken from Numerical Recipes.
C         Modified     11/ 5/93 More convenient for Tables.
C
      INTEGER tls_loc, Search_r
      INTEGER JLow, JHigh, JM
      INTEGER len, inc, i_row
      LOGICAL Ascend
C-------------------------------------------------------------------------------
C
      JLow = ind_r(1)
C
C Check that ind_r(1) is a valid guess. (Otherwise just call Search)
      IF ((JLow.EQ.0) .OR. (JLow.GT.nrows)) THEN
          Locate_r = Search_r( nrows, el1, el2, rkey, ind_r, k_rows )
          RETURN
          END IF
C
      len    = (tls_loc(el2) - tls_loc(el1(1)))/4
      Ascend = el1(1+len*(nrows-1)).GT.el1(1)
C
C Now find JLow and JHigh that bracket rkey.
      inc = 1
#ifdef Linux
      IF ((rkey.GE.el1(1+len*(JLow-1))) .EQV. Ascend) THEN
#else
      IF ((rkey.GE.el1(1+len*(JLow-1))) .EQ. Ascend) THEN
#endif
 1        JHigh = JLow + inc
          IF (JHigh.GT.nrows) THEN
              JHigh = nrows+1
#ifdef Linux
            ELSE IF ((rkey.GE.el1(1+len*(JHigh-1))) .EQV. Ascend) THEN
#else
            ELSE IF ((rkey.GE.el1(1+len*(JHigh-1))) .EQ. Ascend) THEN
#endif
              JLow = JHigh + 1
              inc  = inc + 1
              GO TO 1
            END IF
        ELSE
          JHigh = JLow
 2        JLow  = JHigh - inc
          IF (JLow.LT.1) THEN
              JLow = 0
#ifdef Linux
            ELSE IF ((rkey.LT.el1(1+len*(JLow-1))) .EQV. Ascend) THEN
#else
            ELSE IF ((rkey.LT.el1(1+len*(JLow-1))) .EQ. Ascend) THEN
#endif
              JHigh = JLow
              inc   = inc + 1
              GO TO 2
            END IF
        END IF
C
C Have rkey bracketed by JLow and JHigh.
C Rest is the same as Search.
 10   CONTINUE
      IF (JHigh.GT.JLow+1) THEN
          JM = (JHigh+JLow)/2
#ifdef Linux
          IF ((rkey.GT.el1(1+len*(JM-1))) .EQV. Ascend) THEN
#else 
          IF ((rkey.GT.el1(1+len*(JM-1))) .EQ. Ascend) THEN
#endif
              JLow = JM
            ELSE
              JHigh = JM
            END IF
          GO TO 10
          END IF
C
C  If  Ascend we have el1(Jlow) <  rkey <= el1(JHigh)
C  If ~Ascend we have el1(Jlow) => rkey >  el1(JHigh)
      IF (Ascend) THEN
          ind_r(1) = JHigh
          JM  = JHigh
          inc = + 1
        ELSE
          ind_r(1) = JLow
          JM  = JLow
          inc = - 1
        END IF
      IF (el1(1+len*(ind_r(1)-1)).NE.rkey) THEN
          Locate_r = tls_sort_sn_nomatch_cv
          RETURN
        ELSE
          IF (k_rows.GT.1) THEN
              i_row = 2
              JM = JM + inc
              DO WHILE (el1(1+len*(JM-1)).EQ.rkey)
                 IF (i_row.GT.k_rows) THEN
                    Locate_r  = tls_sort_sn_listshort_cv
                    RETURN
                    END IF
                 ind_r(i_row) = JM
                 i_row = i_row + 1
                 JM     = JM + inc
                 END DO
              END IF
        END IF
      Locate_r = tls_sort_normal_cv
      RETURN
      END
