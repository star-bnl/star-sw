* $Id: tls_id_offsets.F,v 1.1 1998/03/24 00:42:07 fisyak Exp $
* $Log: tls_id_offsets.F,v $
* Revision 1.1  1998/03/24 00:42:07  fisyak
* Clean
*
      INTEGER FUNCTION tls_Id_Offsets( nrows, el1, el2, index, isize )
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
C 9/8/95 Add code to handle 0,1 row tables   djp
      INTEGER tls_loc
      INTEGER loop, rc, ind, istride, id
C
C First zero index.
      DO loop = 1,isize
         index(loop) = 0
         END DO
      IF (nrows.EQ.0) THEN
          tls_Id_Offsets = tls_sort_normal_cv
          RETURN
        ELSE IF (nrows.EQ.1) THEN
          index(loop) = 0
          tls_Id_Offsets = tls_sort_normal_cv
          RETURN
        END IF
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
      tls_Id_Offsets = rc
      RETURN
      END
