       INTEGER*4 FUNCTION ev0_direct(
     1            svt_hit_h,      svt_hit ,
     2            tpc_hit_h,      tpc_hit,
     3            track_h,        track,
     4            vertex_h,       vertex,
     5            svt_track_h,    svt_track,
     6            tpc_track_h,    tpc_track,
     7            tte_eval_h,     tte_eval,
     8            globtrk_h,      globtrk )
C>-----------------------------------------------------------------------
C   Input arguments
C

C     svt_hit_h:   geant svt hit structure header
C     svt_hit:     geant svt hit structure
C     tpc_hit_h:   geant tpc hit structure header
C     tpc_hit:     geant tpc hit structure
C     track_h:     geant track table headrer
C     track:       geant track table
C     vertex_h:    geant vertex table header
C     vertex_h:    geant vertex table
C     svt_track_h: header for svt_track table
C     svt_track:   svt track table, minal filling
C     tpc_track_h: header for tpc track table
C     tpc_track:   tpc_track table, minimal filling
C     tte_eval_h:  header for tte_eval table
C     tte_eval:    tpc tracking evaluation table
C     globtrk_h:   global track headed
C     globtrk :    global track table
C
C   Output arguments : 
C     globtrk - global tracks
C     svt_track 
C     tpt_track
C     tte_eval
C   svt_track, tpt_track and tte_eval are only filled so can be used in
C    evaluation  routine ev0_eval2
C     iok = user status code
C     
C   DESCRIPTION  : Directly fills globtrk with all geant tracks
C   Error conditions    : bad if hit.id .ne. row position 
C<----------------------------------------------------------------------
      IMPLICIT NONE
#include "ev0_direct.inc"


      integer i,j,k,nok,charge,svtnok,tpcnok
      real pt,rr,raddeg,pi

      pi = acos(-1.0)
      raddeg = 180./pi
      
      nok = 0
      svtnok = 0
      tpcnok = 0
      globtrk_h.nok  = 0
      do i=1,globtrk_h.maxlen
         globtrk(i).sflag = -1
         globtrk(i).id_emc = 0
         globtrk(i).id_hypo_pid = 0
         globtrk(i).id_part = 0
         globtrk(i).last_row = 0
         globtrk(i).chisq = 0
         globtrk(i).length = 0
         globtrk(i).xlast(1) = 0
         globtrk(i).xlast(2) = 0
         globtrk(i).xlast(3) = 0
         do j=1,15
            globtrk(i).cov(j) = 0
         enddo
      enddo
C
C  set return code to normal success

      EV0_DIRECT=STAFCV_OK

      DO  i=1,track_h.nok
         rr = 1000.
         j = 1
         k = 0
         if( abs(track(i).charge) .eq. 1 ) then
            if( track(i).n_svt_hit .ne. 0) then
               do while (j .le. svt_hit_h.nok)
                  
                  if(  svt_hit(j).track_p .eq. track(i).id ) then

                     if (  sqrt((svt_hit(j).x(1))**2.+
     >                    (svt_hit(j).x(2))**2) .lt. rr) then
                        rr =   sqrt((svt_hit(j).x(1))**2.+
     >                       (svt_hit(j).x(2))**2)
                        k = j
                     endif
                  endif
                  j = j +1
               enddo
               if( k .ne. 0) then
                  svtnok = svtnok + 1
                  svt_track(svtnok).id = svtnok
                  svt_track(svtnok).id_globtrk = track(i).id
                  svt_track(svtnok).id_mctrack = track(i).id
                  nok = nok + 1    
                  charge=track(i).charge
                  
                  pt = sqrt((svt_hit(k).p(1))**2.
     >                 +(svt_hit(k).p(2))**2.)
                  
                  globtrk(nok).id   = track(i).id
                  globtrk(nok).ndegf = track(i).n_svt_hit + 
     >                 track(i).n_tpc_hit
                  globtrk(nok).icharge= int(charge)
                  globtrk(nok).r0   = sqrt((svt_hit(k).x(1))**2.
     >                 +(svt_hit(k).x(2))**2.)
                  globtrk(nok).phi0 = atan2( svt_hit(k).x(2), 
     >                 svt_hit(k).x(1) )*raddeg
                  globtrk(nok).z0   = svt_hit(k).x(3)
                  globtrk(nok).psi  = atan2( svt_hit(k).p(2),
     >                 svt_hit(k).p(1) )*raddeg
                  globtrk(nok).tanl = (svt_hit(k).p(3) )/pt
                  globtrk(nok).invpt= 1./pt
                  globtrk(nok).sflag = 1
                  globtrk(nok).id_global_pid = track(i).ge_pid
                  globtrk(nok).id_pver = track(i).next_vtx_trk_p
                  
                  if( track(i).n_tpc_hit .gt. 0 ) then
                     tpcnok = tpcnok+1
                     tpc_track(tpcnok).id = tpcnok
                     tpc_track(tpcnok).q = track(i).charge
                     tpc_track(tpcnok).invp = 1./pt
                     tpc_track(tpcnok).id_globtrk = track(i).id
                     
                     tte_eval(tpcnok).rtrk = tpcnok
                     tte_eval(tpcnok).mtrk = track(i).id
                  endif
                  
               endif
            endif
            if( track(i).n_tpc_hit .ne. 0 .and. k .eq. 0) then
               j = track(i).hit_tpc_p
               rr = 999999999.0
               do while (j .ne. 0)
                  
                  if( j .ne. tpc_hit(j).id) then
                     write(6,*) 'PANIC now'
                  endif
                  if(  sqrt((tpc_hit(j).x(1))**2.+
     >                 (tpc_hit(j).x(2))**2) .lt. rr) then
                     rr =   sqrt((tpc_hit(j).x(1))**2.+
     >                    (tpc_hit(j).x(2))**2)
                     k = j
                     j = tpc_hit(j).next_tr_hit_p
                  else
                     j = 0
                  endif
               enddo

               nok = nok + 1    
               charge=track(i).charge
               
               pt = sqrt((tpc_hit(k).p(1))**2.
     >              +(tpc_hit(k).p(2))**2.)
               
               globtrk(nok).id   = track(i).id
               globtrk(nok).ndegf = track(i).n_svt_hit + 
     >              track(i).n_tpc_hit
               globtrk(nok).icharge= int(charge)
               globtrk(nok).r0   = sqrt((tpc_hit(k).x(1))**2.
     >              +(tpc_hit(k).x(2))**2.)
               globtrk(nok).phi0 = atan2( tpc_hit(k).x(2), 
     >              tpc_hit(k).x(1) )*raddeg
               globtrk(nok).z0   = tpc_hit(k).x(3)
               globtrk(nok).psi  = atan2( tpc_hit(k).p(2),
     >              tpc_hit(k).p(1) )*raddeg
               globtrk(nok).tanl = (tpc_hit(k).p(3) )/pt
               globtrk(nok).invpt= 1./pt
               globtrk(nok).sflag = 1
               globtrk(nok).id_global_pid = track(i).ge_pid
               globtrk(nok).id_pver = track(i).next_vtx_trk_p
               tpcnok = tpcnok+1
               tpc_track(tpcnok).id_globtrk = track(i).id
               tpc_track(tpcnok).id = tpcnok
               tpc_track(tpcnok).q = track(i).charge
               tpc_track(tpcnok).invp = 1./pt
               tte_eval(tpcnok).rtrk = tpcnok
               tte_eval(tpcnok).mtrk = track(i).id
               
            endif
            if((globtrk_h.maxlen).le.nok .or.
     >           svt_track_h.maxlen .le. svtnok .or.
     >           tpc_track_h.maxlen .le. tpcnok .or.
     >            tte_eval_h.maxlen .le. tpcnok) goto 10
         endif
      enddo
         
 10   CONTINUE

      globtrk_h.nok   = nok
      svt_track_h.nok = svtnok
      tpc_track_h.nok = tpcnok
      tte_eval_h.nok = tpcnok
 999  CONTINUE

      RETURN
      END
      







