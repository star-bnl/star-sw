* $Id: tls_search_index_r.F,v 1.2 1998/04/09 15:18:34 fisyak Exp $
* $Log: tls_search_index_r.F,v $
* Revision 1.2  1998/04/09 15:18:34  fisyak
* tls bug fixes from Duncan J. Prindle
*
* Revision 1.1  1998/03/24 00:42:11  fisyak
* Clean
*
      INTEGER FUNCTION tls_Search_Index_r(
     +                     nrows, el1, el2, rkey,
     +                     j_table_off, k_rows, index, j_index_off )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C Inputs
      INTEGER nrows          ! Number of rows in the table.
      REAL    el1(*)         ! Element we consider to be a key (in first row)
      REAL    el2            ! Element we consider to be a key (in second row)
      INTEGER index(*)       ! Array el1 will have key sorted by index.
      REAL    rkey           ! Key we are searching for.
C Outputs
      INTEGER k_rows         ! Number of keys found. On input this is taken to
                             ! be the dimension of j_table_off.
      INTEGER j_table_off(k_rows)
                             ! Indices of rows which contain rkey in the table.
      INTEGER j_index_off(k_rows)
                             ! Offsets into index of rows containing rkey.
                             ! This is primarily useful as something to hand
                             ! to Search_Near_Index.
                             ! NOTE THAT j_index_off(1) CAN BE 0 IF
                             !    rkey < el1(1) FOR ASCENDING TABLE
                             ! OR rkey > el1(nrows) FOR DESCENDING TABLE.
C
C           
C        Return Values
C           tls_sort_normal_cv (S):       Normal return
C           tls_sort_si_nomatch_cv (I):   rkey not found in specified column.
C           tls_sort_si_listshort_cv (W): j_table_off too short to contain
C                                         all rows with rkey.
C           
C        Comments: These functions use a bisection search algorithm to search
C                  an ordered list (Table) for a given element (key). The
C                  offset into the list is returned. We return the index
C                  j_table_off such that
C                      el1(j_table_off) <= rkey < el1(j_table_off+1) 
C                  if table is sorted in increasing order or
C                      el1(j_table_off) > rkey => el1(j_table_off+1) 
C                  if the table is sorted in decreasing order.
C
C                  This algorithm assumes that nothing is known about where
C                  key will be found. If the approximate position of key is
C                  known it will be more efficient to use Search_Near.
C 
C                  When these are implemented in Fortran 90 the generic 
C                  name will be tls_Search_Index.
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,  12/ 4/91
C                       Taken from Numerical Recipes.
C         Modified     11/ 5/93 More convenient for Tables.
C                       9/ 8/95 Add code to handle 0,1 row tables   djp
C                       4/ 8/98 Ascend was using el1(index(1)). Wrong!!
C
      INTEGER tls_loc
      INTEGER JLow, JHigh, JM
      INTEGER len, inc, i_row
      LOGICAL Ascend
C-------------------------------------------------------------------------------
C
      IF (nrows.EQ.0) THEN
          tls_Search_Index_r = tls_sort_s_nomatch_cv
          k_rows = 0
          RETURN
        ELSE IF (nrows.EQ.1) THEN
          IF (el1(1).EQ.rkey) THEN  ! I ingore the index assuming it is ok.
              tls_Search_Index_r = tls_sort_normal_cv
              j_index_off(1) = 1
              k_rows = 1
              RETURN
            ELSE
              tls_Search_Index_r = tls_sort_s_nomatch_cv
              k_rows = 0
              RETURN
            END IF
        END IF
          
      JLow   = 0
      JHigh  = nrows + 1
      len    = (tls_loc(el2) - tls_loc(el1(1)))/4
      Ascend = el1(1+len*(index(nrows)-1)).GT.el1(1+len*(index(1)-1))
   10 CONTINUE
      IF (JHigh.GT.JLow+1) THEN
          JM = (JHigh+JLow)/2
#ifdef Linux
          IF ((rkey.GT.el1(1+len*(index(JM)-1))) .EQV. Ascend) THEN
#else
          IF ((rkey.GT.el1(1+len*(index(JM)-1))) .EQ. Ascend) THEN
#endif
              JLow = JM
            ELSE
              JHigh = JM
            END IF
          GO TO 10
          END IF
C
C  If  Ascend we have el1(index(Jlow))  < rkey <= el1(index(JHigh))
C  If ~Ascend we have el1(index(Jlow)) => rkey >  el1(index(JHigh))
      IF (Ascend) THEN
          j_index_off(1) = JLow
          inc = + 1
        ELSE
          j_index_off(1) = JHigh
          inc = - 1
        END IF
      IF (el1(1+len*(index(j_index_off(1)+inc)-1)).EQ.rkey) THEN
          j_index_off(1) = j_index_off(1) + inc
          j_table_off(1) = index(j_index_off(1))
          JM  = j_index_off(1)
          IF (k_rows.GT.1) THEN
              i_row = 2
              JM = JM + inc
              DO WHILE (el1(1+len*(index(JM)-1)).EQ.rkey)
                 IF (i_row.GT.k_rows) THEN
                    tls_Search_Index_r  = tls_sort_si_listshort_cv
                    RETURN
                    END IF
                 j_index_off(i_row) = JM
                 j_table_off(i_row) = index(JM)
                 i_row = i_row + 1
                 JM     = JM + inc
                 END DO
              k_rows = i_row - 1
              END IF
          tls_Search_Index_r = tls_sort_normal_cv
        ELSE
          IF (j_index_off(1).GE.nrows) THEN
              j_table_off(1) = nrows+1
            ELSE IF (j_index_off(1).LE.0) THEN
              j_table_off(1) = 0
            ELSE
              j_table_off(1) = index(j_index_off(1))
            END IF
          tls_Search_Index_r = tls_sort_si_nomatch_cv
          k_rows = 0
        END IF
      RETURN
      END
