* $Id: tpt_extend_segments.F,v 1.7 1999/09/30 07:52:44 sakrejda Exp $
* $Log: tpt_extend_segments.F,v $
* Revision 1.7  1999/09/30 07:52:44  sakrejda
* Tracks that cannot be refit are marked as bad. Also after segment extension
* all the hits that belong to that track are marked again even if number
* of points did not change because new points might be added and old removed.
* Tis cured a problem of nfit>nrec (tested on 4 events).
*
* Revision 1.6  1999/09/14 00:55:45  liq
* use tpt_track_pointers.inc instead of tpt_pointers.inc in order to used by C++ codes
*
* Revision 1.5  1999/07/19 19:15:54  genevb
* Use StMessMgr
*
* Revision 1.4  1999/05/05 21:52:10  sakrejda
* local arrays converted to dynamic ones (with malloc)
*
* Revision 1.3  1998/03/23 18:25:55  sakrejda
* tpg_pad-plane removed from the tpt module, it was not used there anyway
*
* Revision 1.2  1998/01/27 00:35:44  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_EXTEND_SEGMENTS(hit_h,hit,track_h,track,
     >                             tpar_h,tpar,trk,
     >                             next_hit,slice_loc,slice_pnt,
     >                             last_track,ipass)
C>--------------------------------------------------------------------
C
C TPT_EXTEND_SEGMENTS -  extends segments starting from the longest one
C
C DESCRIPTION: Routine extends segments by placing more points on tracks
C It uses a helix with parameters fitted during the previous step as a
C track model. It starts from the longest segment. Long segments are 
C allowed to take points from the short segments. Shorter segments can 
C be destroyed in this process.
C
C INPUT ARGUMENTS:
C  hit_h     - header for the tphit (tpc hits) table
C  hit      - rows of the tphit table
C  track_h   - header for the tptrack (reconstructed tracks) table
C  track    - rows of the tptrack table
C  tpar_h    - header for the tptpar (tracking parameters) table
C  tpar     - tracking parameters table
C  loc_hit  - pointers to the tphit table sorted according to the
C             row/sec field
C  tpstart  - pointers to first hits in every row/sector
C  tpcount  - count of hits for every row/sector
C  trk      - trk      - structure of reconstructed segments
C
C OUTPUT ARGUMENTS:
C  track_h   - header for the tptrack (reconstructed tracks) table
C  track    - rows of the tptrack table
C  trk      - structure of reconstructed segments
C AUTHOR:
C
C<--------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE

#include "PAM.inc"

#include "tpt_pars.inc"
#include "tpt_track.inc"
#include "tcl_tphit.inc"
#include "tpt_track_pointers.inc"
#include "tpt_mxdim.inc"

C______________________________________________________________________
      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)
      RECORD /table_head_st/    track_h
      RECORD /tpt_track_st/     track(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /tpt_track_pointers_st/   trk(*)
C_____________________________________________________________________
C
C     Local variables
      INTEGER next_hit(*),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic)
      INTEGER i,ipos,j,k,l,ld,lc,lvmax,last_track
      REAL    tmp(mxtrack)
      INTEGER iret,ng,naf,save_nhit,save_nfit,laf(mxpnt)
      INTEGER save_ipnt(mxpnt),save_flag,tpt_project
      INTEGER itrack, ihit(mxpnt),ipass
      REAL    save_p(9)
      LOGICAL lfound
      CHARACTER*132 m132
C_____________________________________________________________________
C
C     collect information on number of hits per track to use it 
C     for selecting tracks for extentions
C
      do i=last_track+1,track_h.nok
          tmp(i)=trk(i).nhit
      end do
C
      do i=1,hit_h.nok
          if(hit(i).track.gt.(last_track+1)*1000)
     >    hit(i).track=-hit(i).track
      end do
C     find the longest track segment and extend it...
      do i=1,track_h.nok-last_track
         ipos = lvmax(tmp(last_track+1),track_h.nok-last_track)+
     >          last_track

         if(int(tmp(ipos)).ne.trk(ipos).nhit) then
           write(m132,*) 'TPT_EXTEND_SEGMENTS - accounting messed up',
     >     tmp(ipos),trk(ipos).nhit
           call StWarning(m132)
         endif

         if(int(tmp(ipos)).ge.mxrow)then
C              mark points as used , don't do anything
               do j=1,trk(ipos).nhit
                  hit(trk(ipos).ipnt(j)).track=
     >            abs(hit(trk(ipos).ipnt(j)).track)
               end do
C              mark track as extended
               tmp(ipos) = -1.0
         else if(int(tmp(ipos)).lt.tpar(ipass).minfit) then
C              too many hits were removed from this track......
               tmp(ipos)=-1.0
               trk(ipos).flag=-10.0
               track(ipos).flag=-10
               do j=1,trk(ipos).nhit
                  hit(trk(ipos).ipnt(j)).track = 0
               end do
         else
C              save current content
               save_nhit = trk(ipos).nhit
               do j=1,save_nhit
                  save_ipnt(j)=trk(ipos).ipnt(j)
               enddo
               do j=1,9
                  save_p(j)=trk(ipos).p(j)
               enddo
               save_nfit = trk(ipos).nfit
               save_flag = trk(ipos).flag
C              mark all the points on this track as used
               do j=1,trk(ipos).nhit
                  hit(trk(ipos).ipnt(j)).track=
     >            abs(hit(trk(ipos).ipnt(j)).track)
               end do
               iret = tpt_project(ipos,hit_h,hit,track_h,track,
     >                tpar_h,tpar,trk,next_hit,
     >                slice_loc,slice_pnt,ipass)
C              mark candidate as used
               tmp(ipos) = -1.0
               if(iret.ne.STAFCV_OK) then
c                 could not refit the track, flag it as invalid an release hits.
                  track(ipos).flag = -66
                  trk(ipos).flag = -66
                  trk(ipos).nhit = save_nhit
                  do j=1,save_nhit
                    trk(ipos).ipnt(j)=save_ipnt(j)
                  enddo
C                 free the remaining points
                  do j=1,trk(ipos).nhit
                    hit(trk(ipos).ipnt(j)).track= 0
                  end do
               else
C                 update affected chains
                  naf = 0
                  do j=1,trk(ipos).nhit
                     ld=trk(ipos).ipnt(j)
                     itrack = abs(hit(ld).track)/1000
                     if(itrack.ne.ipos.and.itrack.ne.0) then
                       if(tmp(itrack).eq.-1.0) then
                         write(m132,*)'TPT_EXTEND_SEGMENTS - accounting problems II',
     >                   hit(ld).track,ipos,itrack,trk(itrack).flag
                         call StWarning(m132)
                         read(5,*) tpar(ipass).debug(9)
                       endif
C                      check which hit it was on the track
                       lc = mod(abs(hit(ld).track),1000)
                       lfound = .false.
                       k = 1
                       do while (.not.lfound.and.k.le.naf)
                         if(laf(k).eq.itrack) lfound = .true.
                         k=k+1
                       end do
                       if(.not.lfound) then
                         naf = naf+1
                         laf(naf)=itrack
                       endif
                       trk(itrack).ipnt(lc) = 0
                     endif
C                    mark it as used
                     hit(ld).track = ipos*1000+j
                  end do
C                 loop through all the affected chains and update 
C	          track and hit information
C                 reset number of hits on a track
                  do k=1,naf
                     ng = 0
                     do l=1,trk(laf(k)).nhit
                        if(trk(laf(k)).ipnt(l).ne.0) then
                          ng = ng+1
                          ihit(ng) = trk(laf(k)).ipnt(l)
                        endif
                     end do
                     trk(laf(k)).nhit = ng
c                    mark that the track needs to be re-fit
                     trk(laf(k)).nfit = -1
                     do l=1,trk(laf(k)).nhit
                          trk(laf(k)).ipnt(l) = ihit(l)
                          hit(ihit(l)).track = -(laf(k)*1000+l)
                     end do
                     tmp(laf(k))=real(ng)
                  end do
               endif
         endif
      end do
      tpt_extend_segments=STAFCV_OK
      end
