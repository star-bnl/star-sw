* $Id: tfs_displ_event.F,v 1.1 1998/01/27 01:47:20 fisyak Exp $
* $Log: tfs_displ_event.F,v $
* Revision 1.1  1998/01/27 01:47:20  fisyak
* Split sources
*
      SUBROUTINE TFS_DISPL_EVENT( par_h, par, tphit_h, tphit )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     Input Arguments:
C       par_h   = header for TFS fspar table
C       par    = rows of TFS fspar table
C       tphit_h = header of TCL tphit table
C       tphit  = rows of TCL tphit table
C
C     Output Arguments:
C       values in tphit table
C
C     Functional Description:
C       Displaces separate events along the beam direction according
C       to the current bunch crossing number ix. This routine is used
C       to overlap multiple pp events in the high luminosity mode of
C       pp running at RHIC. The average number of events per crossing
C       is calculated in TFS_INI form the beam parameter table.
C
C     Author:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       17-Mar-1992
C
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tfs_fspar.inc"
#include "tcl_tphit.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st / par_h
      RECORD / tfs_fspar_st / par(*)
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
      REAL DZXING, EVPXNG
      INTEGER NXINGS, NXTRIG
      COMMON /EVMERG/ DZXING, NXINGS, NXTRIG, EVPXNG
C     DZXING - z separation between beam bunches
C     NXINGS - number of beam crossings per TPC live time
C     NXTRIG - the number of the triggered crossing
C     EVPXNG - average number of events per beam crossing

C Local variables
      integer i, j, istart, iend, k, ix

      real dz

C=========================== Begin Executable Code =======================

C     now do the displacement
      do i = 1, par(1).nsect
         do j = 1, par(1).nrow
            istart = irow(1,j,i)
            iend   = irow(1,j,i) + irow(2,j,i) - 1
            do k = istart, iend

C     decode crossing number and compare to triggered event
               ix = tphit(k).flag / 100
               tphit(k).flag = mod( tphit(k).flag, 100)
               dz = (nxtrig - ix)*dzxing
               
               if( tphit(k).z .ge. 0. ) then
                  tphit(k).z = tphit(k).z + dz
C     discard if hit moved through membrane
                  if( tphit(k).z .lt. 0. ) then
C     flag hit for discarding
                     tphit(k).flag = 9
                  endif
               else
                  tphit(k).z = tphit(k).z - dz
C     discard if hit moved through membrane
                  if( tphit(k).z .gt. 0. ) then
C     flag hit for discarding
                     tphit(k).flag = 9
                  endif
               endif
C     discard if hit moved outside tpc
               if( abs( tphit(k).z ) .ge. par(1).tpclen ) then
C     flag hit for discarding
                  tphit(k).flag = 9
               endif
            enddo
         enddo
      enddo
C     
      return
      end
