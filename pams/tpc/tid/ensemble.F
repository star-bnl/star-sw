***************************************************************************
*
*$Id: ensemble.F,v 1.1 2000/06/14 15:17:21 aihong Exp $
*
*Author: Aihong Tang & Richard Witt. Kent State U.
*
***************************************************************************
*$Log: ensemble.F,v $
*Revision 1.1  2000/06/14 15:17:21  aihong
*split tde.F & and new module for ensemble truncation
*
***************************************************************************
      INTEGER FUNCTION ensemble(tdpar_h,tdpar,hit_h,hit,
     >   track_h,track,tpcdedx_h, tpcdedx,truncpoint_h, truncpoint)

      IMPLICIT NONE

C-----------------------------------------------------------------------
C
C Input arguements
C      hit_h   = header for the tphit table
C      hit     = rows of the tphit table
C      track_h = header for the tptrack table (reconstructed tracks)
C      track   = rows of the tptrack table
C      tdpar_h = header for the tdeparm (parameters) table
C      tdpar   = rows of the tdeparm table
C
C Output arguements:
C      ndedx = number of points used in dE/dx for each reconstructed
C              track in the tptrack table
C      dedx  = dE/dx got by ensemble truncation for each reconstructed 
C              track in the tptrack table
C       
C
C Functional description:
C      A module that re-calculate de/dx based on ensemble truncation,
C         taking the truncated mean de/dx as input.
C      This module have to run *AFTER* tde module. 
C pl. note that track.dedx will be ensemble dedx after this module called
C-----------------------------------------------------------------------

#include "ensemble.inc"


      STRUCTURE /tde_local/
                REAL length
                REAL charge
                REAL adedx
                REAL sorta_dedx
                INTEGER row
      END STRUCTURE


      INTEGER    maxNhits
      PARAMETER (maxNhits=200)

      STRUCTURE /trk_hit_local/  !store all hit index for a trk for qk access
                 INTEGER hit_idx(maxNhits)
                 INTEGER Nhit    ! actual # members in hit_idx.
      END STRUCTURE


      INTEGER     ndemax, nlocmax, nloctrc  
      PARAMETER  (ndemax=1000)
      PARAMETER  (nlocmax=500000)
      PARAMETER  (nloctrc=30000)
      
      RECORD  /tde_local/      dedx(ndemax)
      RECORD  /trk_hit_local/  trackhitmap(nloctrc)
    
      INTEGER loc_hit(nlocmax),loc_track(nloctrc)
      INTEGER iret
      INTEGER i,itrack,j,trackrow,hitrow,NGoodHits
      INTEGER imsg1, imsg2, imsg3,truncopt_recover
      REAL    tuncPoint
      REAL    dedx1recover, dedx2recover

C     function
      INTEGER calcuaverage

      CHARACTER*132 m132

      DATA    imsg1 /0/, imsg2 /0/, imsg3 /0/
C     following block just for write out hits
C-----------------------------------------------
      REAL pt,mtm
      INTEGER writehits   ! 0 not write.
C------------------------------------------------





C-----------------------------------------------------

*      call message('ENSEMBLE_DEDX-I: Execution started',1,imsg1)

C Abort if there are no TPC hits to work with
      if (hit_h.nok.eq.0) then
        call message('ENSEMBLE_DEDX-W: No TPC hitss.'// 
     >                 'ENSEMBLE aborted.',1,imsg2)
         ensemble=STAFCV_BAD
         return
      endif

C Abort if there are too many TPC hits to work with
      if (hit_h.nok.gt.nlocmax) then
        call message('ENSEMBLE_DEDX-W: Too many TPC hits.'// 
     >                 'ENSEMBLE aborted.',1,imsg2)
         ensemble=STAFCV_BAD
         return
      endif

C Abort if there are no TPC tracks to work with
      if (track_h.nok.eq.0) then
        call message('ENSEMBLE_DEDX-W: No TPC tracks.'//
     >                 'ENSEMBLE aborted.',1,imsg3)
         ensemble=STAFCV_BAD
         return
      endif

C Abort if there are too many TPC tracks to work with
      if (track_h.nok.gt.nloctrc) then
        call message('ENSEMBLE_DEDX-W: too many TPC tracks.'// 
     >                 'ENSEMBLE aborted.',1,imsg3)
         ensemble=STAFCV_BAD
         return
      endif

C Information write out
      write(m132,*)  'ENSEMBLE - averaging methods: ',
     >     tdpar(1).averaging_method    
      call StInfo(m132)        
      if (tdpar(1).usemerge.ne.0) then
           call StInfo('ENSEMBLE - using merged hits!!!')
      endif

C Establish pointers to tracks
      do i=1,track_h.nok
       loc_track(track(i).id)=i
      end do

C Initialize the track-hit map.
C init all Nhit=0 all hit_idx=-999     
      do i=1, nloctrc
         trackhitmap(i).Nhit=0
       do j=1,maxNhits
         trackhitmap(i).hit_idx(j)=-999
       end do
      end do

C fill the track-hit map. index is the track row  hit_idx is idex of 
C of hits belongs to this track.
      do i=1,hit_h.nok
         itrack=hit(i).track/1000
         trackrow=loc_track(itrack) ! track's physical row in table
       if (trackrow.lt.track_h.nok.and.
     >     trackhitmap(trackrow).Nhit.lt.maxNhits.and.
     >    (hit(i).dedx.gt.0.0).and.
     >     ((hit(i).flag.eq.0).or.(tdpar(1).usemerge.ne.0))) then
        if ((trackhitmap(trackrow).Nhit).lt.maxNhits) then
         trackhitmap(trackrow).Nhit=trackhitmap(trackrow).Nhit+1
         trackhitmap(trackrow).hit_idx(trackhitmap(trackrow).Nhit)=i
        endif
       endif
      end do

C Zero dedx table
      do i=1, ndemax
         dedx(i).length=0.0
         dedx(i).charge=0.0
         dedx(i).adedx=0.0
         dedx(i).sorta_dedx=0.0
         dedx(i).row=-999
      end do

C Loop over tpcdedx table, find ensemble fields. fill dedx table.

      do i=1, tpcdedx_h.nok
       if ((tpcdedx(i).method).eq.2) then
        if ((tpcdedx(i).dedx(1)).ne.0.and.
     >       (tpcdedx(i).ndedx).gt.0) then
           trackrow=loc_track(tpcdedx(i).id_track)
         if (trackrow.lt.track_h.nok) then
           NGoodHits=0
         do j=1, trackhitmap(trackrow).Nhit
           hitrow=trackhitmap(trackrow).hit_idx(j)


          if ((hit(hitrow).dedx-tpcdedx(i).dedx(1)).
     >  lt.(truncpoint(1).data)) then

           NGoodHits=NGoodHits+1
           dedx(NGoodHits).row=mod(hit(hitrow).row,100)
           dedx(NGoodHits).length=hit(hitrow).dq  !this is from tde.F,bad name.
           dedx(NGoodHits).adedx =hit(hitrow).dedx
           dedx(NGoodHits).charge=hit(hitrow).q
           dedx(NGoodHits).sorta_dedx=hit(hitrow).dedx ! no use for ensemble, but fill it anyway 
          endif
         end do

C  store dedx for this track for recovering (do not touch truncated mean stuff)
          dedx1recover=track(trackrow).dedx(1)
          dedx2recover=track(trackrow).dedx(2)

C           set the truncopt to 1 so that all hits are used
           truncopt_recover=tdpar(1).truncopt
           tdpar(1).truncopt        = 1  ! use all dedx points

          iret=calcuaverage(tdpar_h,tdpar,
     >                  track_h,track,trackrow,
     >                  NGoodHits,NGoodHits,dedx)

C           refresh tpcdedx
          tpcdedx(i).ndedx=NGoodHits
          tpcdedx(i).dedx(1)=track(trackrow).dedx(1)
          tpcdedx(i).dedx(2)=track(trackrow).dedx(2)

C           recovering values that changed by calcuaverage.
          tdpar(1).truncopt      = truncopt_recover  ! recover it
          track(trackrow).dedx(1)=dedx1recover
          track(trackrow).dedx(2)=dedx2recover

         endif  !if (trackrow.lt.track_h.nok)
        endif   !if ((tpcdedx(i).dedx(1)).ne.0.and.
       endif    !if ((tpcdedx(i).method).eq.2) then
      end do

C---------------following block just for write hits 4 qkensemble analysis
      writehits=0

      if (writehits.eq.1) then

      open (unit=10,
     > file='/star/rcf/scratch/aihong/ensemble/hitstable.txt',
     >  status='old')

      do i=1, tpcdedx_h.nok
       if ((tpcdedx(i).method).eq.2) then
        if ((tpcdedx(i).dedx(1)).ne.0.and.
     >       (tpcdedx(i).ndedx).gt.0) then
           trackrow=loc_track(tpcdedx(i).id_track)
         if (trackrow.lt.track_h.nok) then
           NGoodHits=0
           if (track(trackrow).invp.ne.0.0) then
           pt=1.0/track(trackrow).invp
           mtm=pt*sqrt(1.0+(track(trackrow).tanl)**2)
           write (10,*) track(trackrow).dedx(1),
     > track(trackrow).ndedx,trackhitmap(trackrow).Nhit,
     > pt,mtm

             do j=1, trackhitmap(trackrow).Nhit
               hitrow=trackhitmap(trackrow).hit_idx(j)
               write (10,*) hit(hitrow).dedx,hit(hitrow).dq
             end do

            end if
           end if
          end if
         end if
        end do

        end if
C----------------------------------------------------------------------------



      ensemble=STAFCV_OK
  
      end










