* $Id: tpt_form_segments.F,v 1.6 1998/05/26 04:15:13 sakrejda Exp $
* $Log: tpt_form_segments.F,v $
* Revision 1.6  1998/05/26 04:15:13  sakrejda
* message type set to E to limit printing
*
* Revision 1.5  1998/04/09 23:08:23  sakrejda
* write to unit 6 replaced by a message call
*
* Revision 1.4  1998/03/29 05:04:59  sakrejda
* Protection agains overflow in trk checked against mxtrack
*
* Revision 1.2  1998/01/27 00:35:47  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_FORM_SEGMENTS(hit_h,hit,
     >                                   track_h,track,
     >                                   tpar_h,tpar,
     >                                   loc_hit,tpstart,tpcount,trk,
     >                                   next_hit,slice_loc,slice_pnt,
     >	                                 ipass,what)
C>--------------------------------------------------------------------
C
C TPT_FORM_SEGMENTS - segment formation with the linear extrapolation
C
C DESCRIPTION:
C Start from the outermost row in every sector and work your way
C towards the TPC center. Find all the 3-hit links from every hit
C that has not been used so far (tpt_find_root) and try to build track
C segments starting from them. For every point select 
C the best reconstructed
C fragment and forget about all the others. Use the linear extrapolation
C when calculating predictions.
C
C INPUT ARGUMENTS:
C  tpar_h    - header for the tptpar (tracking parameters) table
C  tpar     - tracking parameters table
C  hit_h     - header for the tphit (tpc hits) table
C  hit      - rows of the tphit table
C  loc_hit  - pointers to the tphit table sorted according to the 
C	      row/sec field
C  tpstart  - pointers to first hits in every row/sector
C  tpcount  - count of hits for every row/sector
C
C OUTPUT ARGUMENTS:
C  track_h   - header for the tptrack (reconstructed tracks) table
C  track    - rows of the tptrack table
C  trk      - structure with # of entries equal to number of generated
C             track segments. Carries pointers to the hits on tracks
C             and preliminary track parameters
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE
C______________________________________________________________________
C
#include "PAM.inc"
#include "tpt_pars.inc"
#include "tpt_track.inc"
#include "tcl_tphit.inc"

#include "tpt_pointers.inc"
#include "tpt_mxdim.inc"
#include "tpt_root.inc"
#include "tpt_mxhit.inc"
C______________________________________________________________________

      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)
      RECORD /table_head_st/    track_h
      RECORD /tpt_track_st/     track(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)

      RECORD /track_pointers/   trk(*)
C______________________________________________________________________
C
C     Local variables
      RECORD /track_pointers/ saved_trk
      RECORD /track_root/     root(mxroot)
      INTEGER next_hit(tpt_mxhit)
      INTEGER slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector,mxrow,mxslic)
      INTEGER loc_hit(*)
      INTEGER tpstart(mxrow,mxsector)
      INTEGER tpcount(mxrow,mxsector)
      INTEGER i,j,k,iend,nroot,iret,l,ld,ient,ntrk,itest
      INTEGER tpt_find_root,tpt_make_segment,tpt_make_segment_uv
      INTEGER tpt_load_track,tpt_cross_fact
      REAL    xold, yold, cf,q1,q2,what
      LOGICAL saved,better
      INTEGER klc,ldpnt,ipass
c
c     for messages
      integer idI1, idI2, idI3
      character*132 m132(2)  ! And some message-generating space.
      data idI1, idI2, idI3 /3*0/
      save idI1, idI2, idI3
C______________________________________________________________________
      itest = 0
      do i=tpar(ipass).irow_root_beg,tpar(ipass).irow_root_end,-1
         do j=1,mxsector
            iend=tpstart(i,j)+tpcount(i,j)-1
            do klc=tpstart(i,j),iend
               k=loc_hit(klc)
               if(hit(k).track.lt.1) then
C              find all the possible 3-hit links starting from this point
               iret=tpt_find_root(k,hit_h,hit,tpar_h,tpar,
     >                            nroot,root,
     >                            next_hit,slice_loc,slice_pnt,ipass)
c
C              if no roots were found skip it.....
               if(nroot.gt.0) then               
C
C              no saved tracks yet....
               saved = .false.
               if(track_h.nok.lt.mxtrack) then
                  ntrk = track_h.nok + 1
                  do l=1,nroot
C                   look for a track starting from this root
                    trk(ntrk).nhit = 3
                    do ld=1,3
                       trk(ntrk).ipnt(ld)=root(l).hit(ld)
                    end do
                    trk(ntrk).flag=0
C                   start track segment from each root
                    if(what.gt.0.) then
                    iret = tpt_make_segment_uv(ntrk,hit_h,hit,tpar_h,
     >                     tpar,trk,
     >                     next_hit,slice_loc,slice_pnt,ipass)
                    else
                    iret = tpt_make_segment(ntrk,hit_h,hit,tpar_h,
     >                     tpar,trk,
     >                     next_hit,slice_loc,slice_pnt,ipass)
                    endif
C                   check whether a track was successfully fitted
                    better = (trk(ntrk).flag.gt.0)
     >                       .and.(iret.eq.STAFCV_OK).and.nroot.gt.1
                    if(better.and.saved) then
C                     if there was one before that, compare the two
C                     and decide which one was better
                      q1=saved_trk.p(8)/(saved_trk.nfit-3)+
     >                   saved_trk.p(9)/(saved_trk.nfit-2)
                      q2=trk(ntrk).p(8)/(trk(ntrk).nfit-3)+
     >                   trk(ntrk).p(9)/(trk(ntrk).nfit-2)
                      better=((q2 - q1).lt.0.5.and.
     >                trk(ntrk).nfit.gt.saved_trk.nfit).or.
     >                ((q2 - q1).lt.0.and.
     >                trk(ntrk).nfit.eq.saved_trk.nfit)
CCCCCCCCCCCCCC        better=trk(ntrk).nfit.gt.saved_trk.nfit
                    endif
                    if(better.and.l.lt.nroot) then
C                       save if the new track is the only one or 
C	                better than the saved one
                        saved_trk.nhit=trk(ntrk).nhit
                        saved_trk.nfit=trk(ntrk).nfit
                        saved_trk.flag = trk(ntrk).flag
                        do ld=1,saved_trk.nhit
                           saved_trk.ipnt(ld)=trk(ntrk).ipnt(ld)
                        end do
                        do ld=1,9
                           saved_trk.p(ld) = trk(ntrk).p(ld)
                        end do
                        saved = .true.
                    endif
                  end do ! end of loop on l (number of roots)
               else
                  write(m132(1),*)'TPTMAKES1-E1 too many track candidates'
                  write(m132(2),*)'track table size', mxtrack
                  call Message( m132, 2, idI1 )
               endif ! end of check on room in the tracks table

C              if there was a good track load it and mark all the hits
C              memorize the final best segment
               if(saved) trk(ntrk).flag=saved_trk.flag
               if(trk(ntrk).flag.gt.0) then
                 track_h.nok = ntrk
                 trk(ntrk).flag = ipass+10*int(what+1.0)
                 if(saved) then
                   trk(ntrk).nhit=saved_trk.nhit
                   do ld=1,saved_trk.nhit
                     trk(ntrk).ipnt(ld) = saved_trk.ipnt(ld)
                   end do
                 endif
                 do ld=1,trk(ntrk).nhit
                   ient = trk(ntrk).ipnt(ld)
                   if(ient.gt.hit_h.nok.or.ient.lt.1) then
                     write(m132(1),*) 'TPTMAKES2-E1 WRONG POINTER*',ient
                     call Message( m132, 1, idI2 )
                   endif
                   if(hit(ient).track.ne.0) then
                     write(m132(1),*) 'TPTMAKES3-E1 hit overwritten'
     >                                ,ient,hit(ient).track
                     call Message( m132, 1, idI3 )
                   endif
                   hit(ient).track = ntrk*1000+ld
                 end do
                 ldpnt = trk(ntrk).nhit
                 ld = trk(ntrk).ipnt(ldpnt)
                 track(ntrk).hitid = hit(ld).id
                 xold = hit(ld).x
                 yold = hit(ld).y
                 do ld=trk(ntrk).nhit-1,1,-1
                    ient = trk(ntrk).ipnt(ld)
                    iret = tpt_cross_fact(hit(ient).row,hit(ient).x,
     >              hit(ient).y,xold,yold,cf,hit(ient).alpha)
                    xold = hit(ient).x
                    yold = hit(ient).y
                 end do
                 hit( trk(ntrk).ipnt(trk(ntrk).nhit)).alpha =
     >           hit( trk(ntrk).ipnt(trk(ntrk).nhit-1)).alpha
                 if(saved) then
                   do ld=1,9
                    trk(ntrk).p(ld) = saved_trk.p(ld)
                   end do
                 endif
                 iret = tpt_load_track(ntrk,track_h,track,trk)
               endif ! end of an if on good track (flag>0)
               endif ! end of an if on nroot>0
               endif ! end of an if on available hit
            end do  ! end of a loop over hits
         end do ! end of a loop over sectors
      end do    ! end of loop over rows
      tpt_form_segments=STAFCV_OK
      end
