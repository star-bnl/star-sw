      INTEGER FUNCTION TPT_AM(tpar_h,tpar,pad_pl_h,pad_pl,
     >                               hit_h,hit,track_h, track)
C
C TPT_AM - main track finding 
C
C DESCRIPTION:
C Sets up corespondence between the hit id and the entry in the tphit
C table( tpt_hit_pnt), then sets up a look up tables so that hits for
C a given row & sector could be found( tpt_row_sect_pnt). Next track
C segments are found(tpt_form_segments)and extended(tpt_extend_segments)
C Final step consists of an attempt to merge reconstructed pieces of
C tracks.
C
C INPUT ARGUMENTS:
C  tpar_h    - header for the tptpar (tracking parameters) table
C  tpar     - tracking parameters table
C  pad_pl_h  - header for the pad_plane (pad_plane geometry) table
C  pad_pl   - pad_plane geometry table
C  hit_h     - header for the tphit (tpc hits) table
C  hit      - rows of the tphit table
C
C OUTPUT ARGUMENTS:
C  track_h   - header for the tptrack (reconstructed tracks) table
C  track    - rows of the tptrack table
C
C<---------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variable.
C
      IMPLICIT NONE

#include "tpt_am.inc"

#include "tpt_pointers.inc"
#include "tpt_mxdim.inc"
#include "tpt_mxhit.inc"

C______________________________________________________________________

C     Local variables
      RECORD /track_pointers/ trk(mxtrack)
C
      INTEGER loc_hit(tpt_mxhit)
C             loc_hit - pointers to the tphit table sorted acc 
C             to row/sec field
      INTEGER tpstart(mxrow,mxsector),tpcount(mxrow,mxsector)
C             tpstart -pointer to the first hit in the row/sector
C             tpcount -# of hits in a given row/sector
      INTEGER tls_Index_Sort_i
C            sort hits in the TPC table according to row/sec field
      INTEGER tpt_row_sect_pnt 
      INTEGER tpt_form_segments
      INTEGER tpt_extend_segments
      INTEGER tpt_merge_segments
      INTEGER iret  !scratch integer
      INTEGER i,ipass
      INTEGER next_hit(tpt_mxhit),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic),last_track
      REAL    time_now,time_all
      COMMON /CPUTIM/ time_all
      REAL    time_sort,time_seguv,time_seg,time_ext,time_merge
      real    xglobal(3), xlocal(3)
      integer tgc_global_to_local,isect
C______________________________________________________________________
C     initialize timing counters
      time_sort = 0
      time_seguv = 0
      time_seg = 0
      time_ext = 0
      time_merge = 0
      ipass=1
      if(tpar(ipass).debug(1).eq.1) then
          write(6,*) 'starting in the main tracking subroutine ...'
      endif

C     set return value to normal success
      tpt_am = STAFCV_OK
C     set counter for number of rows of good data to 0
      track_h.nok = 0
C     preset all the hit(i).track to 0
      do i=1,hit_h.nok
        hit(i).track=0
      end do
      if(hit_h.nok.eq.0) go to 888

C     Sort the TPC hits according to row/sector field
      call timed(time_now)
      time_all = time_all + time_now
      do i=1,hit_h.nok
         xglobal(1)=hit(i).x
         xglobal(2)=hit(i).y
         xglobal(3)=10.0
         iret=tgc_global_to_local(isect,xglobal,xlocal)
         if(isect.gt.12) isect=24-isect
         if(isect.eq.0) isect=12
         hit(i).row=isect*100+mod(hit(i).row,100)
      end do
c
      if(hit_h.nok.eq.1)then
        loc_hit(1)=1
      else
        iret=tls_Index_Sort_i(hit_h.nok,hit(1).row,hit(2).row,loc_hit,
     >  tpt_mxhit)
      endif
      call timed(time_now)
      time_all = time_all + time_now
      time_sort = time_now
c
c     Set up tables so that hit could be found within rows and sectors
      iret=tpt_row_sect_pnt(hit_h,hit,loc_hit,tpstart,tpcount,
     >                                  tpar_h,tpar,
     >                                  next_hit,slice_loc,slice_pnt,
     >                                                        ipass)

      if(iret.ne.0) then
            write(6,*) 'unable to set pointers to hits'
            tpt_am=STAFCV_BAD
            return
      else 
            if(tpar(ipass).debug(1).eq.1) then
               write(6,*) 'pointers set, iret: ',iret
            endif
      endif
C
C     end of initialisation
C
C     Find track segments starting from the outermost row
C     First do segment formation with the conformal mapping.
      call timed(time_now)
      time_all = time_all + time_now
      iret=tpt_form_segments(hit_h,hit,track_h,track,tpar_h,tpar,
     >                pad_pl_h,pad_pl,loc_hit,tpstart,tpcount,trk,
     >                next_hit,slice_loc,slice_pnt,ipass,1.0)
      call timed(time_now)
      time_all = time_all + time_now
      time_seguv = time_now

      if(tpar(ipass).form_xy.eq.1) then
C     Then do the regular segment formation from what is left out.
            call timed(time_now)
            time_all = time_all + time_now
            iret=tpt_form_segments(hit_h,hit,track_h,track,
     >                  tpar_h,tpar,
     >                  pad_pl_h,pad_pl,loc_hit,tpstart,tpcount,trk,
     >                  next_hit,slice_loc,slice_pnt,ipass,0.0)
            call timed(time_now)
            time_all = time_all + time_now
            time_seg = time_now
      endif
      if(iret.ne.STAFCV_OK) then
C           failed while generating segments
            write(6,*) 'problem in segment finding'
            tpt_am=STAFCV_BAD
            return
      endif
      if(tpar(ipass).debug(1).eq.1) then
              write(6,*) 'back from segment finding ...'
      endif
      if(tpar(ipass).extend.eq.1) then
C             extend track segments crossing the sector boundries
              call timed(time_now)
              time_all = time_all + time_now
              iret=tpt_extend_segments(hit_h,hit,track_h,track,
     >                         tpar_h,tpar,pad_pl_h,pad_pl,trk,
     >                         next_hit,slice_loc,slice_pnt,
     >                         0,ipass)
              call timed(time_now)
              time_all = time_all + time_now
              time_ext = time_now
              if(iret.ne.STAFCV_OK) then
C               failed while extending segments
                write(6,*) 'problem in extending segments'
                tpt_am=STAFCV_BAD
                return
              endif
              if(tpar(ipass).merge.eq.1) then
C                 merge pieces of tracks together
                  call timed(time_now)
                  time_all = time_all + time_now
                  iret=tpt_merge_segments(track_h,track,trk,hit_h,hit,
     >                          tpar,tpar_h,ipass)
                  call timed(time_now)
                  time_all = time_all + time_now
                  time_merge = time_now
                  if(iret.eq.STAFCV_BAD) then
                    write(6,*) 'problems in segment merging'
                    tpt_am=STAFCV_BAD
                  endif
              endif
      endif

C     write out timing information
      if( tpar(ipass).debug(1).eq.1 ) then
        write(6,*) 'TIME_SORT  : ',time_sort
        write(6,*) 'TIME_SEGUV : ',time_seguv
        write(6,*) 'TIME_SEG   : ',time_seg
        write(6,*) 'TIME_EXT   : ',time_ext
        write(6,*) 'TIME_MERGE : ',time_merge
      endif

      write(6,*) 'first pass', track_h.nok
      ipass=2
      if(tpar(ipass).pass_on.eq.1) then
      last_track=track_h.nok
C     Set up tables so that hit could be found within rows and sectors
      iret=tpt_row_sect_pnt(hit_h,hit,loc_hit,tpstart,tpcount,
     >                      tpar_h,tpar,
     >                      next_hit,slice_loc,slice_pnt,ipass)

        if(iret.ne.0) then
            write(6,*) 'unable to set pointers to hits'
            tpt_am=STAFCV_BAD
            return
        else 
            if(tpar(ipass).debug(1).eq.1) then
               write(6,*) 'pointers set, iret: ',iret
            endif
        endif
      iret=tpt_form_segments(hit_h,hit,track_h,track,tpar_h,tpar,
     >                pad_pl_h,pad_pl,loc_hit,tpstart,tpcount,trk,
     >                next_hit,slice_loc,slice_pnt,ipass,1.0)
      call timed(time_now)
      write(6,*) 'second pass', track_h.nok,'time_now',time_now
      if(tpar(ipass).form_xy.eq.1) then
C     Then do the regular segment formation from what is left out.
            iret=tpt_form_segments(hit_h,hit,track_h,track,tpar_h,
     >                   tpar,
     >                   pad_pl_h,pad_pl,loc_hit,tpstart,tpcount,
     >                   trk,
     >                   next_hit,slice_loc,slice_pnt,ipass,0.0)
      if(tpar(ipass).extend.eq.1) then
C        extend track segments crossing the sector boundries
         iret=tpt_extend_segments(hit_h,hit,track_h,track,
     >                         tpar_h,tpar,pad_pl_h,pad_pl,trk,
     >                         next_hit,slice_loc,slice_pnt,
     >                         last_track,ipass)
      endif
      endif
      endif
888   continue
      end
