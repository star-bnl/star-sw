C================Create Index list for a table.===========================
      INTEGER FUNCTION Id_Offsets( nrows, el1, el2, index, isize )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C        Inputs
      INTEGER nrows     ! Number of rows in Table.
      INTEGER el1(*)    ! Element of first row to create index for.
      INTEGER el2       ! Element in second row.
      INTEGER isize     ! Length of ioff array. (Optional argument when
                        ! we use Fortran 90.)
 
C        Outputs
      INTEGER index(isize)   ! Offsets into Table. Need isize >= MAX(el)
 
C        Return Values
C           tls_sort_normal_cv (S):      Successful.
C           tls_sort_id_short_cv (E):    index array not long enough.
C           tls_sort_id_multiple_cv (E): More than one row with same key.
 
C        Comments: Normal call would look something like:
C                  CALL Id_Offsets( h.nok, Table(1).id, Table(2).id, index, n )

C                  This function is not technically a sort. For tables in which
C                  the primary key is numbered from 1 to N = h.nok we can
C                  directly fill in the ioff array such that
C                  Table(index(j)) is the row that contains id = j.
C                  If MAX(Table(i).id) > h.nok (i.e. some
C                  of the rows have been deleted or filtered out) some
C                  of the entries in index will be 0.
C                  If MAX(Table(i).id) >> h.nok there will be considerable
C                  wasted space in index and it may be better to use
C                  an index sort and then a search routine to find the
C                  desired offsets.
C
      INTEGER tls_loc
      INTEGER loop, rc, ind, istride, id
C
C First zero index.
      DO loop = 1,isize
         index(loop) = 0
         END DO
C
C Loop through table filling index.
      rc = tls_sort_normal_cv
      ind = 1
      istride = (tls_loc(el2) - tls_loc(el1(1)))/4
      DO loop = 1,nrows
         id = el1(ind)
         IF (id.LE.isize) THEN
             IF (index(id).NE.0) THEN
                 rc = tls_sort_id_multiple_cv
                 END IF
             index(id) = loop
           ELSE
             rc = tls_sort_id_short_cv
           END IF
         ind = ind + istride
         END DO
      Id_Offsets = rc
      RETURN
      END
C========================================================================
      INTEGER FUNCTION Index_Sort_r( nrows, el1, el2, index, ind_size )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C INPUT  VARIABLES:
      INTEGER nrows             ! Length of el1 array.
      REAL    el1(*)            ! Element to sort on (in first row of table)
      REAL    el2               ! Element from second row of table.
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
      REAL    Q
C----------------------------------------------------------------------------
C
C
      IF (ind_size.LT.nrows) THEN
          Index_Sort_r = tls_sort_is_short_cv
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
              Index_Sort_r = tls_sort_normal_cv
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
C========================================================================
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
C========================================================================
      INTEGER FUNCTION Quick_Sort_r( nrows, el1, el2, table )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C
C It would be nice to clean this program up.
C Actual sort of table assuming element to be sorted on is real.
C Taken from Numerical Recipes.
C        Return Values
C           tls_sort_normal_cv (S):         Successful.
C           tls_sort_qs_longrow_cv (E):    temp. row space not long enough.
C           tls_sort_qs_stackshort_cv (E): stack in quicksort too short.
C
      INTEGER M, NSTACK
      REAL    FM, FA, FC, FMI
      PARAMETER (M=7,NSTACK=50,FM=7875.,FA=211.,FC=1663.
     *         ,FMI=1.2698413E-4)
C Inputs:
      INTEGER nrows         ! Number of input rows.
      REAL    el1(*)        ! Element to sort on (from first row of table)
      REAL    el2           ! Element to sort on (from second row of table)
      REAL    table(*)      ! table to be sorted.
C
      INTEGER tls_loc
      INTEGER len, JSTACK, L, LL, I, IR, IQ, J
      REAL    t(200), tel1, FX
      INTEGER ISTACK(NSTACK)
C
      len = (tls_loc(el2) - tls_loc(el1(1)))/4
      IF (len.GT.200) THEN
          Quick_Sort_r = tls_sort_qs_longrow_cv
          RETURN
          END IF
C
      JSTACK=0
      L=1
      IR=nrows
      FX=0.
10    IF(IR-L.LT.M)THEN
        DO 13 J=L+1,IR
          tel1 = el1(1+len*(J-1))
          DO LL = 1,len
             t(LL)=table(LL+len*(J-1))
             END DO
          DO 11 I=J-1,1,-1
            IF(el1(1+len*(I-1)).LE.tel1)GO TO 12
            DO LL = 1,len
               table(LL+len*I)=table(LL+len*(I-1))
               END DO
11        CONTINUE
          I=0
12        CONTINUE
          DO LL = 1,len
             table(LL+len*I)=t(LL)
             END DO
13      CONTINUE
        IF (JSTACK.EQ.0) THEN
            Quick_Sort_r = tls_sort_normal_cv
            RETURN
            END IF
        IR=ISTACK(JSTACK)
        L=ISTACK(JSTACK-1)
        JSTACK=JSTACK-2
      ELSE
        I=L
        J=IR
        FX=MOD(FX*FA+FC,FM)
        IQ=L+(IR-L+1)*(FX*FMI)
        tel1 = el1(1+len*(IQ-1))
        DO LL = 1,len
           t(LL)=table(LL+len*(IQ-1))
           table(LL+len*(IQ-1))=table(LL+len*(L-1))
           END DO
20      CONTINUE
21        IF ((J.GT.0) .AND. (tel1.LT.el1(1+len*(J-1)))) THEN
              J=J-1
              GO TO 21
              ENDIF
          IF(J.LE.I)THEN
              DO LL = 1,len
                 table(LL+len*(I-1))=t(LL)
                 END DO
              GO TO 30
              ENDIF
          DO LL = 1,len
             table(LL+len*(I-1))=table(LL+len*(J-1))
             END DO
          I=I+1
22        IF ((I.LE.nrows) .AND. (tel1.GT.el1(1+len*(I-1)))) THEN
              I=I+1
              GO TO 22
              ENDIF
          IF(J.LE.I)THEN
              I=J
              DO LL = 1,len
                 table(LL+len*(I-1))=t(LL)
                 END DO
              GO TO 30
              ENDIF
          DO LL = 1,len
             table(LL+len*(J-1))=table(LL+len*(I-1))
             END DO
          J=J-1
        GO TO 20
30      JSTACK=JSTACK+2
        IF (JSTACK.GT.NSTACK) THEN
            Quick_Sort_r = tls_sort_qs_stackshort_cv
            RETURN
            END IF
        IF(IR-I.GE.I-L)THEN
          ISTACK(JSTACK)=IR
          ISTACK(JSTACK-1)=I+1
          IR=I-1
        ELSE
          ISTACK(JSTACK)=I-1
          ISTACK(JSTACK-1)=L
          L=I+1
        ENDIF
      ENDIF
      GO TO 10
      END
C========================================================================
      INTEGER FUNCTION Quick_Sort( nrows, el1, el2, table )
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
C           tls_sort_qs_longrow_cv (E):    temp. row space not long enough.
C           tls_sort_qs_stackshort_cv (E): stack in quicksort too short.
C
      INTEGER M, NSTACK
      REAL    FM, FA, FC, FMI
      PARAMETER (M=7,NSTACK=50,FM=7875.,FA=211.,FC=1663.
     *         ,FMI=1.2698413E-4)
C Inputs:
      INTEGER nrows         ! Number of input rows.
      INTEGER el1(*)        ! Element to sort on (from first row of table)
      INTEGER el2           ! Element to sort on (from second row of table)
      INTEGER table(*)      ! table to be sorted.
C
      INTEGER tls_loc
      INTEGER len, JSTACK, L, LL, I, IR, IQ, J
      INTEGER t(200), tel1
      REAL    FX
      INTEGER ISTACK(NSTACK)
C
      len = (tls_loc(el2) - tls_loc(el1(1)))/4
      IF (len.GT.200) THEN
          Quick_Sort = tls_sort_qs_longrow_cv
          RETURN
          END IF
C
      JSTACK=0
      L=1
      IR=nrows
      FX=0.
10    IF(IR-L.LT.M)THEN
        DO 13 J=L+1,IR
          tel1 = el1(1+len*(J-1))
          DO LL = 1,len
             t(LL)=table(LL+len*(J-1))
             END DO
          DO 11 I=J-1,1,-1
            IF(el1(1+len*(I-1)).LE.tel1)GO TO 12
            DO LL = 1,len
               table(LL+len*I)=table(LL+len*(I-1))
               END DO
11        CONTINUE
          I=0
12        CONTINUE
          DO LL = 1,len
             table(LL+len*I)=t(LL)
             END DO
13      CONTINUE
        IF (JSTACK.EQ.0) THEN
            Quick_Sort = tls_sort_normal_cv
            RETURN
            END IF
        IR=ISTACK(JSTACK)
        L=ISTACK(JSTACK-1)
        JSTACK=JSTACK-2
      ELSE
        I=L
        J=IR
        FX=MOD(FX*FA+FC,FM)
        IQ=L+(IR-L+1)*(FX*FMI)
        tel1 = el1(1+len*(IQ-1))
        DO LL = 1,len
           t(LL)=table(LL+len*(IQ-1))
           table(LL+len*(IQ-1))=table(LL+len*(L-1))
           END DO
20      CONTINUE
21        IF ((J.GT.0) .AND. (tel1.LT.el1(1+len*(J-1)))) THEN
              J=J-1
              GO TO 21
              ENDIF
          IF(J.LE.I)THEN
              DO LL = 1,len
                 table(LL+len*(I-1))=t(LL)
                 END DO
              GO TO 30
              ENDIF
          DO LL = 1,len
             table(LL+len*(I-1))=table(LL+len*(J-1))
             END DO
          I=I+1
22        IF ((I.LE.nrows) .AND. (tel1.GT.el1(1+len*(I-1)))) THEN
              I=I+1
              GO TO 22
              ENDIF
          IF(J.LE.I)THEN
              I=J
              DO LL = 1,len
                 table(LL+len*(I-1))=t(LL)
                 END DO
              GO TO 30
              ENDIF
          DO LL = 1,len
             table(LL+len*(J-1))=table(LL+len*(I-1))
             END DO
          J=J-1
        GO TO 20
30      JSTACK=JSTACK+2
        IF (JSTACK.GT.NSTACK) THEN
            Quick_Sort = tls_sort_qs_stackshort_cv
            RETURN
            END IF
        IF(IR-I.GE.I-L)THEN
          ISTACK(JSTACK)=IR
          ISTACK(JSTACK-1)=I+1
          IR=I-1
        ELSE
          ISTACK(JSTACK)=I-1
          ISTACK(JSTACK-1)=L
          L=I+1
        ENDIF
      ENDIF
      GO TO 10
      END
C========================================================================
      INTEGER FUNCTION Search_r( nrows, el1, el2, rkey, ind_r, k_rows )
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
      INTEGER ind_r(k_rows)  ! Indices of rows which contain rkey in the table.
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
          IF ((rkey.GT.el1(1+len*(JM-1))) .EQ. Ascend) THEN
              JLow = JM
            ELSE
              JHigh = JM
            END IF
          GO TO 10
          END IF
C
C  If  Ascend we have el1(Jlow)  < rkey <= el1(JHigh)
C  If ~Ascend we have el1(Jlow) => rkey >   el1(JHigh)
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
          Search_r = tls_sort_s_nomatch_cv
          RETURN
        ELSE
          IF (k_rows.GT.1) THEN
              i_row = 2
              JM = JM + inc
              DO WHILE (el1(1+len*(JM-1)).EQ.rkey)
                 IF (i_row.GT.k_rows) THEN
                    Search_r  = tls_sort_s_listshort_cv
                    RETURN
                    END IF
                 ind_r(i_row) = JM
                 i_row = i_row + 1
                 JM     = JM + inc
                 END DO
              END IF
        END IF
      Search_r = tls_sort_normal_cv
      RETURN
      END
C========================================================================
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
          IF ((key.GT.el1(1+len*(JM-1))) .EQ. Ascend) THEN
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
C========================================================================
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
      IF ((rkey.GE.el1(1+len*(JLow-1))) .EQ. Ascend) THEN
 1        JHigh = JLow + inc
          IF (JHigh.GT.nrows) THEN
              JHigh = nrows+1
            ELSE IF ((rkey.GE.el1(1+len*(JHigh-1))) .EQ. Ascend) THEN
              JLow = JHigh + 1
              inc  = inc + 1
              GO TO 1
            END IF
        ELSE
          JHigh = JLow
 2        JLow  = JHigh - inc
          IF (JLow.LT.1) THEN
              JLow = 0
            ELSE IF ((rkey.LT.el1(1+len*(JLow-1))) .EQ. Ascend) THEN
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
          IF ((rkey.GT.el1(1+len*(JM-1))) .EQ. Ascend) THEN
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
          Locate_r = tls_sort_l_nomatch_cv
          RETURN
        ELSE
          IF (k_rows.GT.1) THEN
              i_row = 2
              JM = JM + inc
              DO WHILE (el1(1+len*(JM-1)).EQ.rkey)
                 IF (i_row.GT.k_rows) THEN
                    Locate_r  = tls_sort_l_listshort_cv
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
C========================================================================
      INTEGER FUNCTION Locate( nrows, el1, el2, rkey, ind_r, k_rows )
C
      IMPLICIT NONE
C
#include "tls_ecvalues.inc"
C
C Inputs
      INTEGER nrows          ! Number of rows in the table.
      INTEGER el1(*)         ! Element we consider to be a key (in first row)
      INTEGER el2            ! Element we consider to be a key (in first row)
      INTEGER rkey           ! Key we are searching for.
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
      INTEGER tls_loc, Search
      INTEGER JLow, JHigh, JM
      INTEGER len, inc, i_row
      LOGICAL Ascend
C-------------------------------------------------------------------------------
C
      JLow = ind_r(1)
C
C Check that ind_r(1) is a valid guess. (Otherwise just call Search)
      IF ((JLow.EQ.0) .OR. (JLow.GT.nrows)) THEN
          Locate = Search( nrows, el1, el2, rkey, ind_r, k_rows )
          RETURN
          END IF
C
      len    = (tls_loc(el2) - tls_loc(el1(1)))/4
      Ascend = el1(1+len*(nrows-1)).GT.el1(1)
C
C Now find JLow and JHigh that bracket rkey.
      inc = 1
      IF ((rkey.GE.el1(1+len*(JLow-1))) .EQ. Ascend) THEN
 1        JHigh = JLow + inc
          IF (JHigh.GT.nrows) THEN
              JHigh = nrows+1
            ELSE IF ((rkey.GE.el1(1+len*(JHigh-1))) .EQ. Ascend) THEN
              JLow = JHigh + 1
              inc  = inc + 1
              GO TO 1
            END IF
        ELSE
          JHigh = JLow
 2        JLow  = JHigh - inc
          IF (JLow.LT.1) THEN
              JLow = 0
            ELSE IF ((rkey.LT.el1(1+len*(JLow-1))) .EQ. Ascend) THEN
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
          IF ((rkey.GT.el1(1+len*(JM-1))) .EQ. Ascend) THEN
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
          Locate = tls_sort_l_nomatch_cv
          RETURN
        ELSE
          IF (k_rows.GT.1) THEN
              i_row = 2
              JM = JM + inc
              DO WHILE (el1(1+len*(JM-1)).EQ.rkey)
                 IF (i_row.GT.k_rows) THEN
                    Locate  = tls_sort_l_listshort_cv
                    RETURN
                    END IF
                 ind_r(i_row) = JM
                 i_row = i_row + 1
                 JM     = JM + inc
                 END DO
              END IF
        END IF
      Locate = tls_sort_normal_cv
      RETURN
      END















