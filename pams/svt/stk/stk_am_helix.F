      INTEGER*4 FUNCTION STK_AM_HELIX(
     1         g2t_track_h,         g2t_track ,
     2               spt_h,               spt ,
     3               vtx_h,               vtx ,
     4             ctrack_h,             ctrack ,
     5        g2t_gepart_h,        g2t_gepart ,
     6               hlx_h,               hlx ) 
      IMPLICIT NONE
C=============================================================================
C     
C     stk_am_helix - calculate kine observables according to helix parameters
C     
C     DSCRIPTION:
C     
C     Calculates the kine observables of all reconstructed SVT tracks. At 
C     this time
C     all tracks are assumed to originate from the main vertex. So the py, 
C     pz and
C     ph angle are calculated after extrapoaltion to the main vertex.
C     
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C     
C     DATE:
C     18-OCT-1993
C     
C     ARGUMENTS:
C     vtx_h    header to table vtx          primary vertex location
C     vtx      rows of table   vtx
C     track_h  header to table track        reconstructed tracks
C     track    rows of table   track
C     g2t_track_h   header to table kine
C     g2t_track     rows of table   kine
C     g2t_gepart_h
C     g2t_gepart
C     hlx_h
C     hlx
C==============================================================================
      
C     argument declaration
C     ===================
#include "stk_am_helix.inc"
      
C     Local Declarations:
C     ===================
      integer  itrack, ikine, ispt, ispt1
      logical  done

      integer   pid, idummy
      real      tanl,  psi,  invp       ! momentum info
      real     konst, bfield
      real     phic, phis, phi0, phi
      real     x1, y1, z1, b, e, x0(3), delphi, r, h,  dels, s, k
      real     ptot, pt, p(3), costh, sinth, cosl, sinl, ph, th
      real   lambda
      real     rndm
      external rndm

      
c     error/message
      
      character msg*60
      integer   lines
      integer   msgid1,msgid2,msgid3
      logical   first
      data      first/.true./
      save      first
      
C     RETURN:
C     =======
C     stafcv_ok    : normal completion
C     stafcv_bad : abort event upon abnormal input
C     
C     FORMAT:
C     =======
 1001 format(1x,a)

C     EXECUTABLE CODE:
C     ================
      WRITE(*,*) 'STK_AM_HELIX: Starting ##############################'

      stk_am_helix = stafcv_ok

c     set error id 
      
c      if (hlx_h.mod_count .eq. 0) then
         hlx_h.nok = 1

         hlx(1).nkine = 1
         hlx(1).thmin = 80.
         hlx(1).thmax = 90.
         hlx(1).phmin = 0. 
         hlx(1).phmax = 45.
         hlx(1).pmin  = 0.2
         hlx(1).pmax  = 0.6
         hlx(1).nspt  = 3
         hlx(1).phinc = 10.
         hlx(1).vtx(1) = 0.
         hlx(1).vtx(2) = 0.
         hlx(1).vtx(3) = 0.
         b = 5. ! kgauss
         konst = 2.99792458e-4
c      end if

c     proceed to generation as requested
      
      b = 5.                    ! kgauss
      konst = 2.99792458e-4

      ispt = 0
      itrack = 0

      x0(1) = hlx(1).vtx(1)
      x0(2) = hlx(1).vtx(2)
      x0(3) = hlx(1).vtx(3)
      vtx(1).x(1) = hlx(1).vtx(1)
      vtx(1).x(2) = hlx(1).vtx(2)
      vtx(1).x(3) = hlx(1).vtx(3)
      vtx(1).cov(1) = 0.
      vtx(1).cov(2) = 0.
      vtx(1).cov(3) = 0.
      vtx(1).cov(4) = 0.
      vtx(1).cov(5) = 0.
      vtx(1).cov(6) = 0.
      vtx(1).t      = 0.
      vtx(1).id     = 1
      vtx(1).flag   = 0

      do  ikine = 1, hlx(1).nkine

         ptot = hlx(1).pmin + 
     >             (hlx(1).pmax - hlx(1).pmin)*rndm(idummy)
         th   = hlx(1).thmin + 
     >             (hlx(1).thmax - hlx(1).thmin)*rndm(idummy)
         ph   = hlx(1).phmin + 
     >             (hlx(1).phmax - hlx(1).phmin)*rndm(idummy)

         p(1) = ptot * sind(th) * cosd(ph)
         p(2) = ptot * sind(th) * sind(ph)
         p(3) = ptot * cosd(th)
         pt   = ptot * sind(th)
         e    = sqrt(ptot*ptot + g2t_gepart(hlx(1).pid).amass**2)
         lambda = 90. - th
         cosl   = cosd(lambda)
         sinl   = sind(lambda)
         tanl   = sinl/cosl
         write(6,*) ' test1:cst:',cosd(lambda),sind(lambda),tand(lambda)
         lambda =  3.1415927*lambda/180. 
         write(6,*) ' test1:cst:',cos(lambda),sin(lambda),tan(lambda)
         lambda = 180.*lambda/3.1415927

         write(6,*) ' helix: 1/p,tanl:',1/pt,tanl

c     fill g2t_track table 

         g2t_track(ikine).id = 1
         g2t_track(ikine).ge_pid = hlx(1).pid
         g2t_track(ikine).start_vertex_p  = 0
         g2t_track(ikine).e      = e
         g2t_track(ikine).p(1)   = p(1)
         g2t_track(ikine).p(2)   = p(2)
         g2t_track(ikine).p(3)   = p(3)
         
c     helix cal

       
         r = pt/(konst*g2t_gepart(hlx(1).pid).charge*B)
         h = - g2t_gepart(hlx(1).pid).charge
	write(6,*) ' helix: r,h:',r,h
         phi0 = ph-90.*h

         delphi = h*hlx(1).phinc 
         dels   = delphi*r/(h*cosl)
         dels   = 3.1415927*dels/180.    ! delphi is in degrees, stupid !!!!!

         write(6,*) ' helix: delphi, dels:',delphi, dels

         itrack = itrack  +  1
         ctrack(itrack).id         = itrack
         ctrack(itrack).id_mctrack = 0
         ctrack(itrack).nspt       = hlx(1).nspt

         do ispt1 = 1, hlx(1).nspt
            phi = ispt1*delphi
            s   = ispt1*dels
            write(6,*) ' helix: phi,s :',phi,s 
            ispt = ispt + 1

            spt(ispt).x(1) = x0(1) + r*(cosd(phi0+phi)-cosd(phi0)) 
            spt(ispt).x(2) = x0(2) + r*(sind(phi0+phi)-sind(phi0)) 
            spt(ispt).x(3) = x0(3) + s*sinl
            spt(ispt).cov(1) = 1.
            spt(ispt).cov(2) = 1.
            spt(ispt).cov(3) = 1.

            ctrack(itrack).spt(ispt1) = ispt
         end do

      end do

c     update nok 

      g2t_track_h.nok        = ikine
      spt_h.nok          = ispt
      ctrack_h.nok       = itrack
      vtx_h.nok          = 1

      WRITE(*,*) 'STK_AM_HELIX: Stopping ##############################'
      STK_AM_HELIX=STAFCV_OK
      RETURN
      END
