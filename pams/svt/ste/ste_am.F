      INTEGER*4 FUNCTION STE_AM(
     1        g2t_gepart_h,        g2t_gepart ,
     2       g2t_svt_hit_h,       g2t_svt_hit ,
     3         g2t_track_h,         g2t_track ,
     4         g2t_event_h,         g2t_event ,
     5        g2t_vertex_h,        g2t_vertex ,
     6               spt_h,               spt ,
     7               vtx_h,               vtx ,
     8            groups_h,            groups ,
     9             track_h,             track ,
     9              kine_h,              kine ,
     9          mcgroups_h,          mcgroups ,
     9           mctrack_h,           mctrack ,
     9            mckine_h,            mckine ,
     9             teval_h,             teval ,
     9              teff_h,              teff ) 
      IMPLICIT NONE
C     SYNOPSIS: 
C     STK evaluator for track reconstruction
C     
C     DESCRIPTION:
C     This routine evaluates the tracking reconstruction performance
C     of the STK package.  It proceeds in two steps:
C     1- In a first steps, the routine search for the percentage of 
C     well reconstructed tracks by looking at the number of points
C     on each reconstructed tracks that actually belong to the
C     monte carlo track. It evaluates the number of well reconstructed
C     tracks relative to the number of "real" tracks. etc etc
C     2- In a second step, the routine evaluates the momentum resolution
C     of the tracks reconstructed with a minimum success.
C
C	Note on the variable names:
c
c	prefixes k : geant track - info is from g2t_track table
c	         r : track reconstructed directly - info from mctrack
c	         c : track candidate info
C     
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C     
C     HISTORY:
C     13-nov-1993  C.A.P. written
C     27-nov-1993  c.a.p. added a table mctrack; development
C     13-mar-1994  c.a.p. restructured, enhanced
C     15-apr-1994  c.a.p. adopted k,r,c prefixes
C     27-apr-1994  c.a.p. added a copy of mctrack to teval table to account
c                         for track finder deficiencies
C     29-AUG-1997 H.Caines removed c (& d) track tables added in
C                 (mc)groups tables for spt-tracks identification 
c
C     ARGUMENTS:
#include "ste_am.inc"

C     Local Declarations: 
c     -------------------
      integer ictrack, idtrack, jspt(10), mctrk
      integer the_spt, iteval1, ispt1, ispt2, ispt3
      integer ispt, idspt, icspt, ig2t_track, imctrk
      integer nrspt, ncspt, ngspt
      integer layer, ngood, ndiff
      integer krows, table_off(20), mctable_off(100),ok
      logical done, finished
      integer evt_cnt/0/
      save    evt_cnt
      real konst /0.00299792458/ ! Conversion to Gev/c*tesla-1*cm-1
      real bmag /0.5/

      real     kptot,  kpt,  kp(3),  kth,  kph,  ksec
      real      kpid,  kdedx,  ky,  keta,  ke

      real     rptot,  rpt,  rp(3),  rth,  rph,  rsec 
      real      rpid,  rdedx,  ry,  reta,  re

      real     cptot,  cpt,  cp(3),  cth,  cph,  csec
      real      cpid,  cdedx,  cy,  ceta

      real    rchic, rchiz
      real    cchic, cchiz
      
      real    sgn                  ! wkw

      real    infinity
      parameter (infinity=1.e14)

C Closest approach variables  H.Caines 5.5.97     
      real twopi,degtorad
      real czdca, crdca, kzdca, krdca, x0, y0, xc, yc, xs, ys
      real hrot, radius,  psic, psisc ,phis, dist
      integer gspt,ii
      
c     keys

      integer nkinemax, nsptmax, ndtrackmax
      parameter (nsptmax=20000)
      parameter (nkinemax=100000)
      parameter (ndtrackmax=100000)
      integer sptkey(nsptmax)
      integer g2t_trackkey(nkinemax)
      integer dtrackkey(ndtrackmax)
      integer loc_g2thit(nkinemax)

      integer   tls_Index_Sort_i, tls_Search_Near_i      
c  integer indexed sort, integer search near nominated row

 
c     error/message
      
      character msg*132
      integer   msg_id(40)/40*0./
      logical   first
      data      first/.true./
      save first

c     wkw
      integer iseed

      iseed=111111

 1001 format(1x,'ste_am_nrhit-f : no',
     &     'rows in g2t_svt_hit')
 1002 format(1x,'ste_am_nrspt-f : no',
     &     'rows in spt')
 1003 format(1x,'t_tracking_eval_nrvtx-f : no rows',
     &     'in vtx')
 1005 format(1x,'ste_am_nrmctrack-f :',
     &     ' no rows in mctrack')
 1006 format(1x,'ste_am_nrmckine-f : ',
     &     ' no rows in mckine')
 1008 format(1x,'ste_am_nrtrack-f : ',
     &     'no rows in track')
 1009 format(1x,'ste_am_nrkine-f : ',
     &     'no rows in kine (ok if no real tracking)')
 1010 format(1x,'ste_am_nrg2t_track-f :',
     &     'no rows in g2t_track')
 1011 format(1x,'ste_am_nrvert-f : ',
     &     'no rows in g2t_vertex')
c2345678901234567890123456789012345678901234567890123456789012
 1012 format(1x,'ste_am_sptoob-e :',
     &     'spt(ispt).id out of boundaries:',2i6)
 1013 format(1x,'ste_am_g2t_trackoob-e :',
     &     'g2t_track(ig2t_track).id out of boundaries:',2i6)
 1014 format(1x,'ste_am_fcn-e : found',
     &     'candidate with nspt=',i10,'; ictrack:',i6)
 1015 format(1x,'ste_am_ndec-w : no ',
     &     'dtrack entry corresponding to ',
     & 'ictrack=',i5,' abevt')
 1016 format(1x,'ste_am_sec-w : dtrack',
     & 'entry corresponding to ictrack=',i5,
     & ' had nspt=',i10,'; idtrack:',i6)
 1017 format(1x,'ste_am_coob-e : ictrack:',i5,
     & ' has o.o.b. id_rtrk:',i5)
 1018 format(1x,'ste_am_itgm-e : iteval > ',
     & 'teval_h.maxlen; teval table overflow')
 1020 format(1x,'ste_am_icgk-e : ictrack >',
     & 'kine_h.maxlen')
 1021 format(1x,'ste_am_icgt-e : ictrack > ',
     & 'track_h.maxlen')
 1022 format(1x,'ste_am_isgs-e : spt(1) > ',
     & 'nsptmax, ictrack: ',i10,5x,i10)
 1023 format(1x,'ste_am_bsptk-e : bad spt',
     & 'key for spt(1) =',2i10)
 1024 format(1x,'ste_am_nmctrk-e : mctrk ',
     & 'has no key for spt(1) =',2i10)
 1025 format(1x,'ste_am_bmctrk-e : bad',
     &     'mctrk key =',2i10)
 1026 format(1x,'ste_am_mctrkoob-e : imctrk:',
     &     i5,' has o.o.b. pid:',i10)
 1027 format(1x,'ste_am_nmctrk-e : ',
     &     'mctrk has no key dtrack =',2i10)
 1028 format(1x,'ste_am_bmctrk-e : ',
     &     'bad dtrack key =',2i10)
 1029 format(1x,'ste_am_ilfspt-f : ',
     &     'invalid layer for spt:',i5)
 1030 format(1x,'ste_am_dtrackoob-e : ',
     &     'dtrack(idtrack).id_mctrack out ',
     &     'of boundaries:',2i6)
 1031 format(1x,'ste_am_icl1-e : ',
     &     'ictrack < 1')
 1032 format(1x,'ste_am_mm-e : ',
     &     'mctrk,ndtrackmax:',2i10)

C     EXECUTABLE CODE:
C     ================
      ste_am = stafcv_ok
      twopi = atan(1.0)*8.0
      degtorad = twopi/360.0

      WRITE(*,*) 'STE_AM: Starting ######################################'

      if (g2t_svt_hit_h.nok.le.0) then    ! no hits in svt
         ste_am = stafcv_bad
         write(msg,1001)
         call message(msg,1,msg_id(1))
         return
      else if (spt_h.nok.le.0) then ! no spt
         ste_am = stafcv_bad
         write(msg,1002)
         call message(msg,1,msg_id(2))
         return
      else if (vtx_h.nok.le.0) then ! no vtx
         ste_am = stafcv_bad
         write(msg,1003)
         call message(msg,1,msg_id(3))
         return
      else if (mctrack_h.nok.le.0) then ! no mctrack
c         ste_am = stafcv_bad
         write(msg,1005)
         call message(msg,1,msg_id(5))
c         return
      else if (mckine_h.nok.le.0) then ! no kine
c         ste_am = stafcv_bad
         write(msg,1006)
         call message(msg,1,msg_id(6))
c         return
c      else if (track_h.nok.le.0) then ! no mctrack
c         ste_am = stafcv_bad
c         write(msg,1008)
c         call message(msg,1,msg_id(8))
c         return
      else if (kine_h.nok.le.0) then ! no kine
c         ste_am = stafcv_bad
         write(msg,1009)
         call message(msg,1,msg_id(9))
c         return
      else if (g2t_track_h.nok.le.0) then ! no g2t_track
         ste_am = stafcv_bad
         write(msg,1010)
         call message(msg,1,msg_id(10))
         return
      else if (g2t_vertex_h.nok.le.0) then ! no vert
         ste_am = stafcv_bad
         write(msg,1011)
         call message(msg,1,msg_id(11))
         return
      else if (g2t_gepart_h.nok.le.0) then ! no g2t_gepart
         ste_am = stafcv_bad
         write(msg,1011)
         call message(msg,1,msg_id(12))
         return
      end if
      
      gspt = tls_Index_Sort_i(g2t_svt_hit_h.nok,g2t_svt_hit(1).id,
     >     g2t_svt_hit(2).id,loc_g2thit,
     >     g2t_svt_hit_h.maxlen)
      

      evt_cnt = evt_cnt + 1

c     reset/establish a spt key
     
      do ispt = 1, nsptmax
         sptkey(ispt) = 0
      end do
      ispt = 0
      do while (ispt.lt.spt_h.nok)
         ispt = ispt + 1
         if (spt(ispt).id .lt. nsptmax) then
            sptkey(spt(ispt).id) = ispt
         else
            write(msg,1012) ispt, spt(ispt).id
            call message(msg,1,msg_id(13))
         end if
      end do

c     reset/establish a kine key
     
      do ig2t_track = 1, nkinemax
         g2t_trackkey(ig2t_track) = 0
      end do
      do ig2t_track = 1, g2t_track_h.nok
         if (g2t_track(ig2t_track).id .lt. nkinemax) then
            g2t_trackkey( g2t_track(ig2t_track).id ) = ig2t_track
         else
            write(msg,1013) ig2t_track, g2t_track(ig2t_track).id
            call message(msg,1,msg_id(14))
         end if
      end do

c     reset/establish a dtrack key
     
      do idtrack = 1, ndtrackmax
         dtrackkey(idtrack) = 0
      end do

      do idtrack = 1, mctrack_h.nok
         if (mctrack(idtrack).id_mctrack .lt.
     &		 ndtrackmax) then
            dtrackkey( mctrack(idtrack).id_mctrack ) 
     & 		= idtrack
         else
            write(msg,1030) idtrack, 
     &       mctrack(idtrack).id_mctrack
            call message(msg,1,msg_id(15))
         end if
      end do

c     wkw: Temporary section for putting secondary/primary info in
c     mctrack table under flag variable
      do idtrack =1, mctrack_h.nok
         if (mctrack(idtrack).flag.ne.0) then ! 0 means less than 3 points
            if (mctrack(idtrack).sec.eq.1) then
               mctrack(idtrack).flag=1 ! primary
            else
               mctrack(idtrack).flag=2 ! secondary
            endif
         endif
      enddo


c     loop on all candidates, for selected tracks, get the
c     number of real points assuming the 1st layer point as
c     the reference point.

      ictrack = 0
      iteval1  = 0
      ngood   = 0    ! count the number of perfect tracks

      teff(1).nrtrktot  = 0
      teff(1).nctrktot  = 0
      teff(1).ngtrktot  = 0

      do ispt = 1, 10
         teff(1).nrtrk(ispt) = 0
         teff(1).nctrk(ispt) = 0
         teff(1).ngtrk(ispt) = 0
         teff(1).ndiff(ispt) = 0
      end do

c     ===============================================================
c     copy the mctrack table to teval table to get complete 
c     perfect track information
c     ===============================================================
      do idtrack = 1, mctrack_h.nok

         rpid    = 0
         rp(1)   = mckine(idtrack).p(1)
         rp(2)   = mckine(idtrack).p(2)
         rp(3)   = mckine(idtrack).p(3)
         rptot   = sqrt(rp(1)**2+rp(2)**2+rp(3)**2)
         rpt     = sqrt(rp(1)**2+rp(2)**2)
         rth     = atan2d(rpt,  rp(3))
         rph     = atan2d(rp(2),rp(1))
         rchic   = mctrack(idtrack).chisq(1)
         rchiz   = mctrack(idtrack).chisq(2)
         rdedx   = mctrack(idtrack).dedx(1)
         nrspt   = mctrack(idtrack).nspt
         rsec    = mctrack(idtrack).sec


         if (rpid.lt.1 .or. rpid.gt.g2t_gepart_h.nok) then
            re      = infinity
            ry      = infinity
         else
            re = sqrt(rptot**2+g2t_gepart(rpid).amass**2)+1.e-6
            ry = 0.5*(log(re+rp(3))-log(re-rp(3)))
         end if
         reta    = -log(tand(rth/2.))

         krows = 20
         table_off(1) = 3*idtrack
         ok = tls_Search_Near_i(mcgroups_h.nok,mcgroups(1).id1,
     >        mcgroups(2).id1,idtrack,table_off,krows)        

         if( krows .gt. 0 .and.
     >        mcgroups(table_off(1)).id1 .eq. idtrack
     >        .and. mcgroups(table_off(1)).ident .eq. 1) then
            
c     use first point as the reference
            jspt(1) = mcgroups(table_off(1)).id2 
            if (jspt(1).lt.1 .or. jspt(1).gt.nsptmax) then
               write(msg,1022) jspt(1),idtrack
               call message(msg,1,msg_id(28))
               goto 200
            end if
         endif
         
          
         ispt = sptkey(jspt(1))
         if (ispt.lt.1 .or. ispt.gt.spt_h.nok) then
            write(msg,1023) jspt(1),ispt
            call message(msg,1,msg_id(29))
            goto 200
         end if
         
         mctrk  = spt(ispt).id_mctrack
         if (mctrk.lt.1 .or. mctrk.gt.nkinemax) then
            write(msg,1024) jspt(1),ispt
            call message(msg,1,msg_id(30))
            imctrk = 0
            kpid    = 0
            kp(1)   = infinity
            kp(2)   = infinity
            kp(3)   = infinity
            kptot   = infinity
            kpt     = infinity
            kth     = infinity
            kph     = infinity
            ke      = infinity
            ky      = infinity
            keta    = infinity
            ksec    = 0
            krdca   = infinity
            kzdca   = infinity
         else
            imctrk = g2t_trackkey(mctrk)
            if (imctrk.lt.1 .or. mctrk.gt.g2t_track_h.nok) then
               write(msg,1025) mctrk,imctrk
               call message(msg,1,msg_id(23))
               imctrk = 0
               kpid    = 0
               kp(1)   = infinity
               kp(2)   = infinity
               kp(3)   = infinity
               kptot   = infinity
               kpt     = infinity
               kth     = infinity
               kph     = infinity
               ke      = infinity
               ky      = infinity
               keta    = infinity
               ksec    = 0
               krdca   = infinity
               kzdca   = infinity
            else

*        try to get the closest approach:
*        first get the track radius
*        hrot is the sense of rotation

               hrot=-sign(1.0,g2t_track(imctrk).charge)
               dist = infinity
               do ii=1,g2t_svt_hit_h.nok
                  if( g2t_svt_hit(ii).track_p .eq. imctrk) then
                     if( g2t_svt_hit(ii).x(1)**2+
     >                    g2t_svt_hit(ii).x(2)**2+
     >                    g2t_svt_hit(ii).x(3)**2 .lt. dist) then
                        dist = g2t_svt_hit(ii).x(1)**2+
     >                    g2t_svt_hit(ii).x(2)**2+
     >                    g2t_svt_hit(ii).x(3)**2
                        gspt = ii
                     endif
                  endif
               enddo

               radius=(sqrt(g2t_svt_hit(gspt).p(1)**2+
     >              g2t_svt_hit(gspt).p(2)**2))/(bmag*konst)
               psic=atan2(g2t_svt_hit(gspt).p(2),g2t_svt_hit(gspt).p(1))
     >              -(hrot*twopi/4.0)
               if( psic.gt.twopi) psic=psic-twopi
               if( psic.lt.0)     psic=psic+twopi
               
*     Calc x and y coords at r0
               
               x0 = g2t_svt_hit(gspt).x(1)
               y0 = g2t_svt_hit(gspt).x(2)
               
*     Calc co-ords of centre of circle
               
               xc=x0 - radius*cos(psic)
               yc=y0 - radius*sin(psic)
               
*     rdca is the radius of closest approach to prim. vertex
               
               krdca=radius-sqrt(xc**2+yc**2)
               phis=atan2(yc,xc)
               if(phis.lt.0) phis=phis+twopi
               
               xs=krdca*cos(phis)
               ys=sin(phis)
               psisc=atan2(ys-yc,xs-xc)
               if(psisc.lt.0) psisc=psisc+(twopi)
*     z= z0 + delta(z), tan(lambda)*delta(s)= delta(z), 
*     delta(s) = r*delta(theta)  (from arclength equation) et voila!!!
               
               kzdca=mctrack(idtrack).z0+
     >              (hrot*radius*(psisc-psic)*g2t_svt_hit(gspt).p(3)/
     >              (sqrt(g2t_svt_hit(gspt).p(1)**2+
     >              g2t_svt_hit(gspt).p(2)**2)))
               
               
c     monte carlo track - geant kine info
               
               kpid    = g2t_track(imctrk).ge_pid
               if( g2t_svt_hit(gspt).track_p .eq. imctrk) then
                  kp(1)   = g2t_svt_hit(gspt).p(1)
                  kp(2)   = g2t_svt_hit(gspt).p(2)
                  kp(2)   = g2t_svt_hit(gspt).p(2)
               else
                  write(6,*) 'space point not from track'
                  kp(1)   = g2t_track(imctrk).p(1)
                  kp(2)   = g2t_track(imctrk).p(2)
                  kp(3)   = g2t_track(imctrk).p(3)
               endif
               kptot   = sqrt(kp(1)**2+kp(2)**2+kp(3)**2)
               kpt     = sqrt(kp(1)**2+kp(2)**2)
               kth     = atan2d(kpt,  kp(3))
               kph     = atan2d(kp(2),kp(1))
               if (kpid.lt.1 .or. kpid.gt.g2t_gepart_h.nok) then
                  write(msg,1026) imctrk,kpid
                  call message(msg,1,msg_id(24))
                  ke      = infinity
                  ky      = infinity
               else
                  ke = sqrt(kptot**2+g2t_gepart(kpid).amass**2)+1.e-6
                  ky = 0.5*(log(ke+kp(3))-log(ke-kp(3)))
               end if
               keta    = -log(tand(kth/2.))
               ksec    =  mctrack(idtrack).sec
            end if
         end if
         
c     wkw: section to put in better momentum (local version only!!!)
c     if (kpt.ne.0.and.kpt.ne.infinity) then
c     if (abs(mctrack(idtrack).invpt).lt.1000) then
c     sign=abs(mctrack(idtrack).invpt)/
c     1          mctrack(idtrack).invpt
c     mctrack(idtrack).invpt=sign/(kpt+gasdev(iseed)*.02*kpt)
c     else
c     mctrack(idtrack).invpt=infinity
c     endif
c     else
c     mctrack(idtrack).invpt=infinity
c     endif
         
         
         
c     store to teval table
         iteval1 = iteval1 + 1  ! (added by wkw in 10/3/96)
         
         teval(iteval1).evt   = evt_cnt
         teval(iteval1).id    = iteval1
c     from g2t_track
         
         teval(iteval1).kpid   = kpid
         teval(iteval1).kp(1)  = kp(1)
         teval(iteval1).kp(2)  = kp(2)
         teval(iteval1).kp(3)  = kp(3)
         teval(iteval1).kptot  = kptot
         teval(iteval1).kpt    = kpt
         teval(iteval1).ksec   = ksec
         teval(iteval1).kth    = kth
         teval(iteval1).kph    = kph
         teval(iteval1).ky     = ky
         teval(iteval1).keta   = keta
         teval(iteval1).kdedx  = kdedx
         teval(iteval1).krdca  = krdca
         teval(iteval1).kzdca  = kzdca

c     from mctrack - perfect tracking assumed

         teval(iteval1).rpid   = rpid
         teval(iteval1).rp(1)  = rp(1)
         teval(iteval1).rp(2)  = rp(2)
         teval(iteval1).rp(3)  = rp(3)
         teval(iteval1).rptot  = rptot
         teval(iteval1).rpt    = rpt
         teval(iteval1).rsec   = rsec
         teval(iteval1).rth    = rth
         teval(iteval1).rph    = rph
         teval(iteval1).ry     = ry
         teval(iteval1).reta   = reta
         teval(iteval1).rdedx  = rdedx
         teval(iteval1).rchisq(1) = mctrack(idtrack).chisq(1)
         teval(iteval1).rchisq(2) = mctrack(idtrack).chisq(2)

c     track candidate - none from this part

         teval(iteval1).cpid   =  -1
         teval(iteval1).cp(1)  =  -1
         teval(iteval1).cp(2)  =  -1
         teval(iteval1).cp(3)  =  -1
         teval(iteval1).cptot  =  -1
         teval(iteval1).cpt    =  -1
         teval(iteval1).csec   =  -1
         teval(iteval1).cth    =  -1
         teval(iteval1).cph    =  -1
         teval(iteval1).cy     =  -1
         teval(iteval1).ceta   =  -1
         teval(iteval1).cdedx  =  -1
         teval(iteval1).crdca   =  -1
         teval(iteval1).czdca   =  -1
 
         ispt1 = sptkey(jspt(1))
         teval(iteval1).x1(1)   = spt(ispt1).x(1)
         teval(iteval1).x1(2)   = spt(ispt1).x(2)
         teval(iteval1).x1(3)   = spt(ispt1).x(3)
         
         if( krows .ge. 2) then
            jspt(2) = mcgroups(table_off(2)).id2
         else
            jspt(2) = 0
         endif
         if (jspt(2).lt.1.or.jspt(2).gt.spt_h.nok) then
            teval(iteval1).x2(1)   = infinity
            teval(iteval1).x2(2)   = infinity
            teval(iteval1).x2(3)   = infinity
         else
            ispt2 = sptkey(jspt(2))
            teval(iteval1).x2(1)   = spt(ispt2).x(1)
            teval(iteval1).x2(2)   = spt(ispt2).x(2)
            teval(iteval1).x2(3)   = spt(ispt2).x(3)
         endif

         if( krows .ge. 3) then
            jspt(3) = mcgroups(table_off(3)).id2
         else
            jspt(3) = 0
         endif
         if (jspt(3).lt.1.or.jspt(3).gt.spt_h.nok) then
            teval(iteval1).x3(1)   = infinity
            teval(iteval1).x3(2)   = infinity
            teval(iteval1).x3(3)   = infinity
         else
            ispt3 = sptkey(jspt(3))
            teval(iteval1).x3(1)   = spt(ispt3).x(1)
            teval(iteval1).x3(2)   = spt(ispt3).x(2)
            teval(iteval1).x3(3)   = spt(ispt3).x(3)
         end if
         
         teval(iteval1).kppid  = 0
         teval(iteval1).nrspt  = nrspt
         teval(iteval1).ncspt  = 0
         teval(iteval1).ngspt  = 0

c Added in recording of nrtrk for all geant tracks not just those
c found H.Caines 3.25.97 
  
         if ( nrspt .le. 10 ) then
            teff(1).nrtrk(nrspt) = teff(1).nrtrk(nrspt) + 1
         else
            write(6,*) 'Too many hits on mctrack',iteval1,
     >          'recording as 10'
            teff(1).nrtrk(10) = teff(1).nrtrk(10) + 1
         endif
      end do ! (idtrack = 1, mctrack_h.nok)

 200  continue

c     start evaluation of the track reconstruction
      
      ictrack = 0
      done = .false.
      do while (.not.done)
         if (iteval1 .gt. teval_h.maxlen) then
            ste_am = stafcv_ok
            write(msg,1018)
            call message(msg,1,msg_id(16))
            done = .true.
            goto 100
         end if

c     reconstructed track (candidate)

         ictrack = ictrack +1
         if (ictrack .lt. 1) then
            ste_am = stafcv_ok
            write(msg,1031)
            call message(msg,1,msg_id(27))
            done = .true.
            goto 100
         else if (ictrack.gt. track_h.nok) then
            ste_am = stafcv_ok
            write(msg,1021)
            call message(msg,1,msg_id(17))
            done = .true.
            goto 100
         else if (ictrack.gt. kine_h.nok) then
            ste_am = stafcv_ok
            write(msg,1020)
            call message(msg,1,msg_id(18))
            done = .true.
            goto 100
         end if

         csec     = track(ictrack).sec
         cpid     = kine(ictrack).pid
         cp(1)   = kine(ictrack).p(1)
         cp(2)   = kine(ictrack).p(2)
         cp(3)   = kine(ictrack).p(3)
         cptot   = sqrt(cp(1)**2+cp(2)**2+cp(3)**2)
         cpt     = sqrt(cp(1)**2+cp(2)**2)
         cth     = atan2d(cpt,  cp(3))
         cph     = atan2d(cp(2),cp(1))
         ceta    = -log(tand(cth/2.))
         cchic   = track(ictrack).chisq(1)
         cchiz   = track(ictrack).chisq(2)
         ncspt   = track(ictrack).nspt
         cdedx   = track(ictrack).dedx(1)
         
*        try to get the closest approach:
*        first get the track radius
*        hrot is the sence of rotation

         hrot=-sign(1.0,track(ictrack).invpt)
         radius=1.0/(track(ictrack).invpt*bmag*konst)
         psic=degtorad*track(ictrack).psi-(hrot*twopi/4.0)
         if(psic.gt.twopi) psic=psic-(twopi)
         if(psic.lt.0)     psic=psic+(twopi)

* Calc x and y coords at r0

         x0=track(ictrack).r0*cos(degtorad*track(ictrack).phi0) 
         y0=track(ictrack).r0*sin(degtorad*track(ictrack).phi0)   
* Calc co-ords of centre of circle
    
         xc=x0 - radius*cos(psic)
         yc=y0 - radius*sin(psic)

* rdca is the radius of closest approach to prim. vertex

         crdca=radius-sqrt(xc**2+yc**2)
         phis=atan2(yc,xc)
         if(phis.lt.0) phis=phis+twopi

         xs=crdca*cos(phis)
         ys=sin(phis)
         psisc=atan2(ys-yc,xs-xc)
         if(psisc.lt.0) psisc=psisc+(twopi)
* z= z0 + delta(z), tan(lambda)*delta(s)= delta(z), 
* delta(s) = r*delta(theta)  (from arclength equation) et voila!!!

         czdca=track(ictrack).z0+hrot*radius*(psisc-psic)*
     >   track(ictrack).tanl

c     search the "real" track info
         
         krows = 20
         table_off(1) = 3*ictrack
         ok = tls_Search_Near_i(groups_h.nok,groups(1).id1,
     >        groups(2).id1,ictrack,table_off,krows)        


         if( krows .gt. 0 .and. groups(table_off(1)).ident .gt. 1
     >        .and. groups(table_off(1)).id1 .eq. ictrack) then

            jspt(1) = groups(table_off(1)).id2
c use first point as the reference
            if (jspt(1).lt.1 .or. jspt(1).gt.nsptmax) then
               write(msg,1022) jspt(1),ictrack
               call message(msg,1,msg_id(20))
               ste_am = stafcv_bad
               done = .true.
               goto 100
            end if
         endif

         ispt = sptkey(jspt(1))
         if (ispt.lt.1 .or. ispt.gt.spt_h.nok) then
            write(msg,1023) jspt(1),ispt
            call message(msg,1,msg_id(21))
            ste_am = stafcv_bad
            done = .true.
            goto 100
         end if
         
         idtrack = dtrackkey(spt(ispt).id_mctrack)
         nrspt = mctrack(idtrack).nspt

         krows = 100
         mctable_off(1) = 3*idtrack
         ok = tls_Search_Near_i(mcgroups_h.nok,mcgroups(1).id1,
     >        mcgroups(2).id1,idtrack,mctable_off,krows)        



c     count the number of good space points by looping
c     on candidate space points and checking if they
c     match the "real" ones.
         
         ngspt = 0              ! reset number of good spt for current track
         if (ncspt.gt.0 .and. ncspt.le.8) then
            do icspt = 1, ncspt 
               the_spt = groups(table_off(icspt)).id2
               finished    = .false.
               idspt   = 0
               do while (.not.finished.and.idspt.lt.nrspt 
     >              .and. idspt .lt. 100)
                  idspt = idspt + 1
                  if (the_spt .eq. mcgroups(mctable_off(idspt)).id2) then
                     finished = .true.
                     ngspt = ngspt + 1
                  end if
               end do
            end do
         else
            write(msg,1014) ncspt, ictrack
            call message(msg,1,msg_id(19))
         end if
                  
c        if (ngspt.eq.3) ngood = ngood + 1
c        wkw: section to put some info in track for debuging, not permanent
         if (ngspt.ge.3) then ! trackable
            if (ngspt.eq.track(ictrack).nspt) then !all hits on candidate good

c              wkw: section to put in better momentum (local version only!!!)
c               if (kpt.ne.0.and.kpt.ne.infinity) then
c                  sign=abs(track(ictrack).invpt)/track(ictrack).invpt
c                  track(ictrack).invpt=sign/(kpt+gasdev(iseed)*.02*kpt)
c               else
c                  track(ictrack).invpt=infinity
c               endif
               if (ngspt.eq.mctrack(idtrack).nspt) then ! candidate contains all hits
                  ngood = ngood + 1
c2345678901234567890123456789012345678901234567890123456789012
                  track(ictrack).flag=
     &			mctrack(idtrack).flag+20 ! assume idtrack=ictrack 
               else
                  track(ictrack).flag=
     & 			mctrack(idtrack).flag+10 ! assume idtrack=ictrack 
               endif                  
               track(ictrack).pid=mctrack(idtrack).pid
            else               
               track(ictrack).flag=0 ! contains bad hits
            endif
         else
            track(ictrack).flag=0 ! not enough good spt to track           
         endif
c        section for piding ghosts as pions
         if (track(ictrack).pid.eq.0) then ! assume pion
            if (abs(track(ictrack).invpt).lt.20) then ! pt > 50 MeV and no bad fits
               sgn=abs(track(ictrack).invpt)/
     &		track(ictrack).invpt
               if (sgn.gt.0) then
                  track(ictrack).pid=8 ! pos pion
               else
                  track(ictrack).pid=9 ! neg pion
               endif
            endif
         endif
               


c     store space point statistics
         
         ndiff = nrspt-ngspt

         if (ncspt.gt.9) ncspt = 9
         if (ngspt.gt.9) ngspt = 9
         if (ndiff.gt.9) ndiff = 9

         teff(1).nctrk(ncspt)  = teff(1).nctrk(ncspt) + 1 ! counts candidates
         teff(1).ngtrk(ngspt+1)  = teff(1).ngtrk(ngspt+1) + 1 ! counts good
         teff(1).ndiff(ndiff+1)  = teff(1).ndiff(ndiff+1) + 1 ! counts diff
         
         if ( teval(idtrack).ncspt .eq. 0) then
            teval(idtrack).ncspt = ncspt
            teval(idtrack).ngspt = ngspt

c     track candidate 

            teval(idtrack).cpid   = cpid
            teval(idtrack).cp(1)  = cp(1)
            teval(idtrack).cp(2)  = cp(2)
            teval(idtrack).cp(3)  = cp(3)
            teval(idtrack).cptot  = cptot
            teval(idtrack).cpt    = cpt
            teval(idtrack).csec   = csec
            teval(idtrack).cth    = cth
            teval(idtrack).cph    = cph
            teval(idtrack).cy     = cy
            teval(idtrack).ceta   = ceta
            teval(idtrack).cdedx  = cdedx
            teval(idtrack).cchisq(1)   =  cchic 
            teval(idtrack).cchisq(2)   = cchiz
            teval(idtrack).crdca = crdca
            teval(idtrack).czdca = czdca

         else
c     There has already been a reconstructed track for this mc_track
c     set id = -id to flag that there are more than 1 tracks.

            write(6,*) ' More than 1 ctrks from mctrk',idtrack

            iteval1 = iteval1 + 1
            teval(idtrack).id    = -idtrack
            teval(iteval1).evt   = -evt_cnt
            teval(iteval1).id    = -iteval1
            teval(iteval1).nrspt = nrspt
            teval(iteval1).ncspt = ncspt
            teval(iteval1).ngspt = ngspt
c     from g2t_track
         
            teval(iteval1).kppid   = teval(idtrack).kppid
            teval(iteval1).kp(1)  = teval(idtrack).kp(1)
            teval(iteval1).kp(2)  = teval(idtrack).kp(2)
            teval(iteval1).kp(3)  = teval(idtrack).kp(3)
            teval(iteval1).kptot  = teval(idtrack).kptot
            teval(iteval1).kpt    = teval(idtrack).kpt
            teval(iteval1).krdca   = teval(idtrack).krdca
            teval(iteval1).kzdca   = teval(idtrack).kzdca         
            teval(iteval1).ksec   = teval(idtrack).ksec
            teval(iteval1).kth    = teval(idtrack).kth
            teval(iteval1).kph    = teval(idtrack).kph
            teval(iteval1).ky     = teval(idtrack).ky
            teval(iteval1).keta   = teval(idtrack).keta
            teval(iteval1).kdedx  = teval(idtrack).kdedx
            teval(iteval1).kpid   = teval(idtrack).kpid
            teval(iteval1).x1(1) = teval(idtrack).x1(1)
            teval(iteval1).x1(2) = teval(idtrack).x1(2)
            teval(iteval1).x1(3) = teval(idtrack).x1(3)
            teval(iteval1).x2(1) = teval(idtrack).x2(1)
            teval(iteval1).x2(2) = teval(idtrack).x2(2)
            teval(iteval1).x2(3) = teval(idtrack).x2(3)
            teval(iteval1).x3(1) = teval(idtrack).x3(1)
            teval(iteval1).x3(2) = teval(idtrack).x3(2)
            teval(iteval1).x3(3) = teval(idtrack).x3(3)

c     from mctrack - perfect tracking assumed

            teval(iteval1).rpid   = teval(idtrack).rpid
            teval(iteval1).rp(1)  = teval(idtrack).rp(1)
            teval(iteval1).rp(2)  = teval(idtrack).rp(2)
            teval(iteval1).rp(3)  = teval(idtrack).rp(3)
            teval(iteval1).rptot  = teval(idtrack).rptot
            teval(iteval1).rpt    = teval(idtrack).rpt
            teval(iteval1).rsec   = teval(idtrack).rsec
            teval(iteval1).rth    = teval(idtrack).rth
            teval(iteval1).rph    = teval(idtrack).rph
            teval(iteval1).ry     = teval(idtrack).ry
            teval(iteval1).reta   = teval(idtrack).reta
            teval(iteval1).rdedx  = teval(idtrack).rdedx
            teval(iteval1).rchisq(1)   = mctrack(ictrack).chisq(1)  
            teval(iteval1).rchisq(2)   = mctrack(ictrack).chisq(2) 


c     track candidate 

            teval(iteval1).cpid   = cpid
            teval(iteval1).cp(1)  = cp(1)
            teval(iteval1).cp(2)  = cp(2)
            teval(iteval1).cp(3)  = cp(3)
            teval(iteval1).cptot  = cptot
            teval(iteval1).cpt    = cpt
            teval(iteval1).csec   = csec
            teval(iteval1).cth    = cth
            teval(iteval1).cph    = cph
            teval(iteval1).cy     = cy
            teval(iteval1).ceta   = ceta
            teval(iteval1).cdedx  = cdedx
            teval(iteval1).cchisq(1)   = cchic 
            teval(iteval1).cchisq(2)   = cchiz
            teval(iteval1).crdca = crdca
            teval(iteval1).czdca = czdca
           
         endif

         if (ictrack.ge.track_h.nok) then
            done = .true.
         end if

 100     continue
      end do ! end of while (.not.done)

c     total track accounting...

      teff(1).nrtrktot = mctrack_h.nok   ! number of tracks through svt
      teff(1).nctrktot = track_h.nok   ! number of track candidates
      teff(1).ngtrktot = ngood          ! number of perfect reconstruction

c     total space points accounting 

      teff(1).nspttot  = spt_h.nok      ! number of space points

      do layer = 1, 10
         teff(1).nspt(layer)    = 0
         teff(1).nsptnot(layer) = 0
      end do

      do ispt = 1, spt_h.nok
         layer = spt(ispt).id_wafer/1000
         if (layer.le.0 .or. layer .gt. 8) then
            write(msg,1029) ispt
            call message(msg,1,msg_id(24))
            ste_am = stafcv_bad
            return
         else
            teff(1).nspt(layer) =  teff(1).nspt(layer) + 1   ! count number spt/layer
            if (spt(ispt).flag .eq. 0) then
               teff(1).nsptnot(layer) =  
     &		teff(1).nsptnot(layer) + 1 ! count number spt/layer
            end if
         end if
         
      end do

c     update the teval table header

         teval_h.nok  = iteval1
         teff_h.nok   = 1

      WRITE(*,*) 'STE_AM: Stopping ######################################'
      STE_AM=STAFCV_OK
      RETURN
      END
