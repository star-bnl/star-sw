* $Id: tfs_hit_sort.F,v 1.1 1998/01/27 01:47:23 fisyak Exp $
* $Log: tfs_hit_sort.F,v $
* Revision 1.1  1998/01/27 01:47:23  fisyak
* Split sources
*
      SUBROUTINE TFS_HIT_SORT( tphit_h, tphit )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C
C     Input arguments:
C
C     Output arguments:
C
C     Functional Description :
C       Hits are sorted according to sector-row. Sets up a local
C       pointer index to the first and last hits in a given row.
C
C     Author:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       10-Mar-1993
C
C     Modified:
C	     By		     	      Date	 	  History
C       Iwona Sakrejda(LBL) 	   02/18/1996      To use the GSTAR tables
C	Dhammika Weerasundara(UW)  03/27/1996      
C
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tcl_tphit.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st / tphit_h
      RECORD / tcl_tphit_st / tphit(*)
C-----------------------------------------------------------------------

      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)
      INTEGER IROW(2,MXROW,MXSECTOR)
      COMMON /TPHPNT/ IROW
C     IROW  - 1 pointer to a row in a sector
C     IROW  - 2 number of hits

C Local variables
      integer ih, jrow, jsect, iret
      integer tls_Quick_Sort_i

C=========================== Begin Executable Code =======================

C     Sort hits according to the encoded sector-row information. Then
C     set up pointers to the beginning and end of each sector-row. This
C     is to be used later to sort each sector-row according to drift.

      iret = tls_Quick_Sort_i(tphit_h.nok,tphit(1).row,
     >     tphit(2).row,tphit)
      call vzero(irow,2*mxrow*mxsector)
      jrow = mod( tphit(1).row, 100 )
      jsect = (tphit(1).row - jrow)/100
      irow(1,jrow,jsect) = 1
      irow(2,jrow,jsect) = irow(2,jrow,jsect) + 1
      do ih = 2, tphit_h.nok
         if (tphit(ih).row .ne. tphit(ih-1).row) then
            jrow = mod( tphit(ih).row, 100 )
            jsect = (tphit(ih).row - jrow)/100
            irow(1,jrow,jsect) = ih
         endif
         irow(2,jrow,jsect) = irow(2,jrow,jsect) + 1
      enddo

      RETURN
      END
