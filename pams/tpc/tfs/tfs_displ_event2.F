* $Id: tfs_displ_event2.F,v 1.3 1999/07/19 19:15:48 genevb Exp $
* $Log: tfs_displ_event2.F,v $
* Revision 1.3  1999/07/19 19:15:48  genevb
* Use StMessMgr
*
* Revision 1.2  1998/12/13 21:36:10  sakrejda
* parameters and constants cleaned up, bmpar removed
*
* Revision 1.1  1998/12/03 23:04:33  sakrejda
* Akio's changes to create pileup events added
*
* Revision 1.1  1998/01/27 01:47:20  fisyak
* Split sources
*
      SUBROUTINE TFS_DISPL_EVENT2( par_h, par, tphit_h, tphit,
     +  tpc_index_h, tpc_index, g2t_hit_h, g2t_hit,
     +  g2t_track_h, g2t_track, g2t_vertex_h, g2t_vertex)
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
#include "tcl_tpc_index.inc"
#include "g2t_tpc_hit.inc"
#include "g2t_track.inc"
#include "g2t_vertex.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st / par_h
      RECORD / tfs_fspar_st / par(*)
      RECORD / table_head_st / tphit_h
      RECORD / tcl_tphit_st / tphit(*)
      RECORD / table_head_st / tpc_index_h
      RECORD / tcl_tpc_index_st /  tpc_index(*) 
      RECORD / table_head_st /  g2t_hit_h
      RECORD / g2t_tpc_hit_st / g2t_hit(*)
      RECORD / table_head_st /  g2t_track_h
      RECORD / g2t_track_st /   g2t_track(*)
      RECORD / table_head_st /  g2t_vertex_h
      RECORD / g2t_vertex_st /  g2t_vertex(*)
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
      integer i, j, istart, iend, k, id0, id1, id2, id3
      real dz

      real drivel, dummy, xlength 
      integer iret, tgc_drift_velocity, tgc_drift_volume_length
      character*132 m132

C=========================== Begin Executable Code =======================
c
c     get the drift velocity
      iret = tgc_drift_velocity(drivel,dummy,dummy)
c     get the drift length
      iret = tgc_drift_volume_length(xlength,dummy,dummy)

C     now do the displacement
      do i = 1, par(1).nsect
         do j = 1, par(1).nrow
            istart = irow(1,j,i)
            iend   = irow(1,j,i) + irow(2,j,i) - 1
            do k = istart, iend

C     Trace back from hits to g2t_hits->g2t_track->g2t_vertex
C     and add time offset of the vertex.
               id0=tpc_index(k).key2
               if(id0.ne.k) then
                  call StWarning('TFS_DISPL_EVENT2 - HELP!!!')
                  call StWarning('TFS_DISPL_EVENT2 - tpc_index.key2 is not in order!!!')
                  write(m132,*) 'TFS_DISPL_EVENT2 - id, key2=',k,id0
                  call StWarning(m132)
               endif
               id1=tpc_index(id0).key1
               id2=g2t_hit(id1).track_p
               id3=g2t_track(id2).start_vertex_p
               dz=g2t_vertex(id3).ge_tof*drivel
               
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
               if( abs( tphit(k).z ) .ge. xlength ) then
C     flag hit for discarding
                  tphit(k).flag = 9
               endif

c               write(*,*) i,j,k,':',id0,id1,id2,id3,dz,':',tphit(k).z,tphit(k).flag

            enddo
         enddo
      enddo
C     
      return
      end
