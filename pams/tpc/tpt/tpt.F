* $Id: tpt.F,v 1.19 2000/03/10 08:14:02 sakrejda Exp $
* $Log: tpt.F,v $
* Revision 1.19  2000/03/10 08:14:02  sakrejda
* Marging for field on and off under the same switch.
*
* Revision 1.18  2000/03/04 00:33:07  sakrejda
* Segment merging added for straigth tracks
*
* Revision 1.17  1999/09/14 00:55:44  liq
* use tpt_track_pointers.inc instead of tpt_pointers.inc in order to used by C++ codes
*
* Revision 1.16  1999/07/31 22:13:07  liq
* set switch on forming segments on UV
*
* Revision 1.15  1999/06/01 22:09:18  sakrejda
* dynamic allocation for the local trk record array
*
* Revision 1.14  1999/05/21 02:46:02  sakrejda
* pointers for malloc initialised - were causing crashes in peripheral events
*
* Revision 1.13  1999/05/05 21:52:08  sakrejda
* local arrays converted to dynamic ones (with malloc)
*
* Revision 1.12  1998/09/01 13:32:33  sakrejda
* restored sector numbering 1-24 in tphit.row
*
* Revision 1.11  1998/06/01 03:15:22  sakrejda
* inconsistency in calls to message fixed, TPT-I3 replaced by TPTI3-E1
*
* Revision 1.10  1998/05/26 04:15:12  sakrejda
* message type set to E to limit printing
*
* Revision 1.9  1998/04/22 15:57:55  sakrejda
* write to unit 6 replaced by message calls
*
* Revision 1.8  1998/03/27 20:50:29  sakrejda
* bug in sector verification removed. (old:isec2=isect, new:isec3=isec2)
*
* Revision 1.7  1998/03/23 18:25:51  sakrejda
* tpg_pad-plane removed from the tpt module, it was not used there anyway
*
* Revision 1.6  1998/03/17 16:58:26  sakrejda
* initialisation of track# and sector flag cleanup put in one loop
*
* Revision 1.5  1998/02/11 19:32:55  sakrejda
* tpt protected agains overflows, tpt_find_slic.F,tpt_row_sect_pnt.F simplified (and tested)
*
* Revision 1.3  1998/02/06 22:03:14  fisyak
* Remove tgc_cross_and_dip.F
*
* Revision 1.2  1998/01/27 00:35:42  fisyak
* Split sources
*
      INTEGER FUNCTION TPT(tpar_h,tpar,hit_h,hit,track_h,track)
C
C TPT - main track finding 
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

#include "tpt.inc"

#include "tpt_track_pointers.inc"
#include "tpt_mxdim.inc"

C______________________________________________________________________
      integer malloc
      integer loc

C     Local variables
      RECORD /tpt_track_pointers_st/ trk(mxtrack)
      POINTER (p_trk,trk)
C

      INTEGER tpstart(mxrow,mxsector)
C             tpstart -pointer to the first hit in the row/sector
      INTEGER tpcount(mxrow,mxsector)
C             tpcount -# of hits in a given row/sector

      INTEGER tpt_row_sect_pnt 
      INTEGER tpt_form_segments
      INTEGER tpt_extend_segments
      INTEGER tpt_merge_segments
      INTEGER tpt_merge_line


      INTEGER iret  !scratch integer
      INTEGER i,ipass
      INTEGER slice_loc(mxsector,mxrow,mxslic)

c     
c     Allocate memory for a local array following Victor's instructions
c     *****************************************************************
      Integer   tpt_mxhit
      Parameter (tpt_mxhit=300000)
      INTEGER   next_hit(tpt_mxhit)
      POINTER   (p_next_hit,next_hit)

      INTEGER   loc_hit(tpt_mxhit)
      POINTER   (p_loc_hit,loc_hit)
C             loc_hit - pointers to the tphit table sorted acc 
C             to row/sec field
c     *****************************************************************
      
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic),last_track
      REAL    time_now,time_all
      REAL    time_sort,time_seguv,time_seg,time_ext,time_merge
      REAL    xlocal(3), bfield(3)
c
c     message-related variables
      character*132 m132(5)  ! And some message-generating space.
      integer idI1, idI2, idI3
      data    idI1, idI2, idI3 /3*0/
      data    xlocal /3*0.0/
      save    idI1, idI2, idI3, xlocal
C______________________________________________________________________
C     initialize timing counters
      time_all=0
      time_sort = 0
      time_seguv = 0
      time_seg = 0
      time_ext = 0
      time_merge = 0
      ipass=1
c     
c     initialize pointers
      p_loc_hit  = 0
      p_next_hit = 0
      p_trk = 0
c
      if(tpar(ipass).debug(1).eq.1) then
          call Msg_Enable( 'TPTI1-E1' )
          call Msg_Enable( 'TPTI2-E1' )
          call Msg_Enable( 'TPTI3-E1' )
          call Msg_Enable( 'TPTROOT1-E1' )
      endif

      write(m132,*) 'TPTI3-E1 starting in the main tracking subroutine ...'
      call Message( m132, 1, idI3)

C     set return value to normal success
      tpt = STAFCV_OK
C     set counter for number of rows of good data to 0
      track_h.nok = 0
C
      if(hit_h.nok.eq.0) go to 999

c
c     make sure that there is enough space in local arrays
c     if not, truncate number of hits to track...
c
      p_loc_hit  = malloc(hit_h.nok*4)
      p_next_hit = malloc(hit_h.nok*4)

C     Sort the TPC hits according to row/sector field.
C     Check whether the right sector was assigned to a hit (geant messes it up
c     sometimes). This check will go away at some point.
C     Hits are divided into 12 volumes so that crossing through the central
C     membrane is smooth.
c     Also since this is a loop over all the hits, reset the track assignement
c     to 0.
c
      call timed(time_now)
      time_all = time_all + time_now
      do i=1,hit_h.nok
C        preset all the hit(i).track to 0
         hit(i).track=0
      end do
c
c     Set up tables so that hit could be found within rows and sectors.
      iret=tpt_row_sect_pnt(hit_h,hit,loc_hit,tpstart,tpcount,
     >         tpar_h,tpar,next_hit,slice_loc,slice_pnt,ipass)

      if(iret.ne.0) then
         write(m132,*) 'TPTI2-E1 unable to set pointers to hits'
         call message(m132, 1, idI2)
         tpt=STAFCV_BAD
         go to 999
      else 
         write(m132,*) 'TPTI2-E1 pointers set, iret: ',iret
         call message(m132, 1,id I2)
      endif

c     allocate temporary storage for tracks
      p_trk = malloc(mxtrack*abs(loc(trk(1))-loc(trk(2))))
C
C     end of initialisation
C
C     Find track segments starting from the outermost row
C     First do segment formation with the conformal mapping.
      if(tpar(ipass).form_uv.gt.0) then
         call timed(time_now)
         time_all = time_all + time_now
         iret=tpt_form_segments(hit_h,hit,track_h,track,tpar_h,tpar,
     >        loc_hit,tpstart,tpcount,trk,
     >        next_hit,slice_loc,slice_pnt,ipass,1.0)
         call timed(time_now)
         time_all = time_all + time_now
         time_seguv = time_now
         print *,'use tpt_form_segments fit uv'
      endif

      if(tpar(ipass).form_xy.eq.1) then
C     Then do the regular segment formation from what is left out.
            call timed(time_now)
            time_all = time_all + time_now
            iret=tpt_form_segments(hit_h,hit,track_h,track,
     >                  tpar_h,tpar,
     >                  loc_hit,tpstart,tpcount,trk,
     >                  next_hit,slice_loc,slice_pnt,ipass,0.0)
            call timed(time_now)
            time_all = time_all + time_now
            time_seg = time_now
      endif
      if(iret.ne.STAFCV_OK) then
C           failed while generating segments
            write(m132,*) 'TPTI2-E1 problem in segment finding'
            call Message(m132, 1, idI2)
            tpt=STAFCV_BAD
            go to 999
      endif
      write(m132,*) 'TPTI3-E1 back from segment finding ...'
      call message(m132, 1, idI3)
c
      if(tpar(ipass).extend.eq.1) then
C             extend track segments crossing the sector boundries
              call timed(time_now)
              time_all = time_all + time_now
              iret=tpt_extend_segments(hit_h,hit,track_h,track,
     >                         tpar_h,tpar,trk,
     >                         next_hit,slice_loc,slice_pnt,
     >                         0,ipass)
              call timed(time_now)
              time_all = time_all + time_now
              time_ext = time_now
              if(iret.ne.STAFCV_OK) then
C               failed while extending segments
                write(m132,*) 'TPTI2-E1 problem in extending segments'
                call message(m132, 1, idI2)
                tpt=STAFCV_BAD
                go to 999
              endif
      endif

      if(tpar(ipass).merge.eq.1) then
C       merge pieces of tracks together
        call timed(time_now)
        time_all = time_all + time_now
        call gufld(xlocal,bfield)
        if(abs(bfield(3)).lt.0.1) then
           iret=tpt_merge_line(track_h,track,trk,hit_h,hit,tpar,tpar_h,ipass)
        else
           iret=tpt_merge_segments(track_h,track,trk,hit_h,hit,tpar,tpar_h,ipass)
        endif
        call timed(time_now)
        time_all = time_all + time_now
        time_merge = time_now
        if(iret.eq.STAFCV_BAD) then
          write(132,*) 'TPTI2-E1 problems in segment merging'
          call message(m132, 1, idI2)
          tpt=STAFCV_BAD
        endif
      endif
c
C     write out timing information
      write(m132(1),*) 'TPTI3-E1 TIME_SORT  : ',time_sort
      write(m132(2),*) '       TIME_SEGUV : ',time_seguv
      write(m132(3),*) '       TIME_SEG   : ',time_seg
      write(m132(4),*) '       TIME_EXT   : ',time_ext
      write(m132(5),*) '       TIME_MERGE : ',time_merge
      call message(m132, 5, idI3)
c
      write(m132,*) 'TPTI3-E1 first pass', track_h.nok
      call message(m132, 1, idI3)

c
c     Second pass starts
      ipass=2
      if(tpar(ipass).pass_on.eq.1) then
      last_track=track_h.nok
C     Set up tables so that hit could be found within rows and sectors
      iret=tpt_row_sect_pnt(hit_h,hit,loc_hit,tpstart,tpcount,
     >                      tpar_h,tpar,
     >                      next_hit,slice_loc,slice_pnt,ipass)

        if(iret.ne.0) then
            write(m132,*) 'unable to set pointers to hits'
            tpt=STAFCV_BAD
            go to 999
        else 
            if(tpar(ipass).debug(1).eq.1) then
               write(m132,*) 'pointers set, iret: ',iret
            endif
        endif
      iret=tpt_form_segments(hit_h,hit,track_h,track,tpar_h,tpar,
     >                loc_hit,tpstart,tpcount,trk,
     >                next_hit,slice_loc,slice_pnt,ipass,1.0)
      call timed(time_now)
      write(m132,*) 'second pass', track_h.nok,'time_now',time_now
      if(tpar(ipass).form_xy.eq.1) then
C     Then do the regular segment formation from what is left out.
            iret=tpt_form_segments(hit_h,hit,track_h,track,tpar_h,
     >                   tpar,loc_hit,tpstart,tpcount,trk,
     >                   next_hit,slice_loc,slice_pnt,ipass,0.0)
      if(tpar(ipass).extend.eq.1) then
C        extend track segments crossing the sector boundries
         iret=tpt_extend_segments(hit_h,hit,track_h,track,
     >                         tpar_h,tpar,trk,
     >                         next_hit,slice_loc,slice_pnt,
     >                         last_track,ipass)
      endif
      endif
      endif
999   continue
      call free(p_next_hit)
      call free(p_loc_hit)
      call free(p_trk)
      end
